{-# LANGUAGE ViewPatterns #-}
module Macros (Macro(..), MacroScope, replaceMacros, macrosAreIdentical) where

import Lexer (PpTokOrWhitespace(..),
              PreprocessingToken(..),
              Whitespace(..),
              PhysicalSourceCharacter(..),
              phase123)

import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.List

data Macro
  = ObjectMacro { macroReplacement :: [PpTokOrWhitespace] }
  | FunctionMacro { macroParams :: [String], isMacroVariadic :: Bool, macroReplacement :: [PpTokOrWhitespace] }

type MacroScope = DM.Map String Macro

type HideSet = DS.Set String
data TokenHS = TokenHS PpTokOrWhitespace HideSet
  deriving Show

-- Skip optional whitespace.
optionalWhitespace :: [TokenHS] -> [TokenHS]
optionalWhitespace (TokenHS (Whitespace _) _:ts) = optionalWhitespace ts
optionalWhitespace ts = ts

replaceMacros :: MacroScope -> [PpTokOrWhitespace] -> [PpTokOrWhitespace]
replaceMacros scope = fromHS . expand . toHS
  where
    expand :: [TokenHS] -> [TokenHS]
    expand [] = []
    expand (ths@(TokenHS t hs):ts)
      | tokIsInHS t hs = ths:expand ts
    expand (TokenHS (macro -> Just (t, ObjectMacro rs)) hs:ts)
      = expand (subst (toHS rs) [] [] (DS.insert t hs) [] ++ ts)
    expand (TokenHS (macro -> Just (t, FunctionMacro ps v rs)) hs:(optionalWhitespace -> (punc -> "("):ts))
      = expand (subst (toHS rs) ps' as (DS.insert t (hs `DS.intersection` hs')) [] ++ ts')
      where (as, hs', ts') = extract (length ps') v ts
            ps' | v = ps ++ ["__VA_ARGS__"]
                | otherwise = ps
    expand (t:ts) = t:expand ts

    subst :: [TokenHS] -> [String] -> [[TokenHS]] -> HideSet -> [TokenHS] -> [TokenHS]
    subst [] _ _ hs os = hsadd hs os
    subst ((punc -> "#"):(optionalWhitespace -> (ident -> Just t):is)) fp ap hs os
      | Just i <- elemIndex t fp
      = subst is fp ap hs (os ++ [stringize (ap !! i)])
    subst ((punc -> "##"):(optionalWhitespace -> (ident -> Just t):is)) fp ap hs os
      | Just i <- elemIndex t fp
      = subst is fp ap hs (os' (ap!!i))
      where os' (optionalWhitespace -> []) = os
            os' as = glue os as
    subst ((punc -> "##"):(optionalWhitespace -> t:is)) fp ap hs os
      = subst is fp ap hs (glue os [t])
    subst ((ident -> Just t):(optionalWhitespace -> hh@(punc -> "##"):is)) fp ap hs os
      | Just i <- elemIndex t fp
      = let (is', os') = get (optionalWhitespace $ ap !! i) is
        in subst is' fp ap hs os'
      where get [] (optionalWhitespace -> (ident -> Just t'):is'') | Just j <- elemIndex t' fp = (is'', ap !! j)
            get [] is'' = (is'', [])
            get as is'' = (hh:is'', as)
    subst ((ident -> Just t):is) fp ap hs os
      | Just i <- elemIndex t fp
      = subst is fp ap hs (os ++ expand (ap !! i))
    subst (t:ts) fp ap hs os = subst ts fp ap hs (os ++ [t])

    hsadd hs = map (\(TokenHS t hs') -> TokenHS t (hs `DS.union` hs'))

    stringize = flip TokenHS DS.empty . lexStringLiteralToken . quoteAndEscape . concatMap stringizeTokOrWhitespace . foldWhitespace . fromHS . reverse . optionalWhitespace . reverse . optionalWhitespace
    glue :: [TokenHS] -> [TokenHS] -> [TokenHS]
    glue (TokenHS (PpTok l) hs:(optionalWhitespace -> [])) (optionalWhitespace -> TokenHS (PpTok r) hs':rs) = TokenHS (concatTokens l r) (hs `DS.intersection` hs'):rs
    glue (l:ls) rs = l:glue ls rs

    macro (PpTok (Identifier s)) = fmap ((,) s) $ DM.lookup s scope
    macro _ = Nothing

toHS = map (flip TokenHS DS.empty)
fromHS = map (\(TokenHS t _) -> t)

foldWhitespace (Whitespace _:Whitespace _:ts) = foldWhitespace (Whitespace Horizontal:ts)
foldWhitespace (Whitespace _:ts) = Whitespace Horizontal:foldWhitespace ts
foldWhitespace (t:ts) = t:foldWhitespace ts
foldWhitespace [] = []

lexStringLiteralToken :: String -> PpTokOrWhitespace
lexStringLiteralToken s = case phase123 $ map PSC s of
  [sl@(PpTok (StringLiteral _))] -> sl
  _ -> error "stringization did not produce a single string literal token"

concatTokens a b = case phase123 . map PSC $ stringizeToken a ++ stringizeToken b of
  [t] -> t
  _ -> error "token paste did not produce a single token"

stringizeTokOrWhitespace (Whitespace _) = " "
stringizeTokOrWhitespace (PpTok t) = stringizeToken t

stringizeToken (HeaderName s) = s
stringizeToken (Identifier s) = s
stringizeToken (PpNumber s) = s
stringizeToken (CharacterLiteral s) = s
stringizeToken (UserDefinedCharacterLiteral s) = s
stringizeToken (StringLiteral s) = s
stringizeToken (UserDefinedStringLiteral s) = s
stringizeToken (PreprocessingOpOrPunc s) = s
stringizeToken (Other c) = [c]

quoteAndEscape = ('"':) . (++"\"") . concatMap clean
  where clean '"' = ['\\', '"']
        clean '\\' = ['\\', '\\']
-- FIXME DR1709: clean '\n' = ['\\', 'n']
        clean c = [c]

punc (TokenHS (PpTok (PreprocessingOpOrPunc s)) _) = s
punc _ = ""

ident (TokenHS (PpTok (Identifier s)) _) = Just s
ident _ = Nothing

extract :: Int -> Bool -> [TokenHS] -> ([[TokenHS]], HideSet, [TokenHS])
extract n v [] = error "missing ) at end of macro arguments"
-- FIXME: This does not match the spec: we should apparently reject function-like macro invocations with 0 parameters
extract 0 v (ths@(punc -> ")"):ts) = ([], hs, ts)
  where (TokenHS _ hs) = ths
extract 0 False _ = error "too many macro arguments"
extract 1 v (ths@(punc -> ")"):ts) = ([[]], hs, ts)
  where (TokenHS _ hs) = ths
-- FIXME: nesting of ().
extract n v ((punc -> ")"):ts) = error "too few macro arguments"
extract n v (p@(punc -> "("):ts) = ((p:ps ++ a):as, hs, ts')
  where (ps, rs) = extractParens ts
        (a:as, hs, ts') = extract n v rs
        extractParens (p@(punc -> "("):ts) = (p:ps ++ ps', rs')
          where (ps, rs) = extractParens ts
                (ps', rs') = extractParens rs
        extractParens (p@(punc -> ")"):ts) = ([p], ts)
        extractParens (t:ts) = (t:ps, rs)
          where (ps, rs) = extractParens ts
        extractParens [] = error "missing ) within macro arguments"
extract 1 False ((punc -> ","):ts) = error "too many macro arguments"
extract n v ((punc -> ","):ts) | n /= 1 = ([]:as, hs, ts')
  where (as, hs, ts') = extract (n-1) v ts
extract n v (t:ts) = ((t:a):as, hs, ts')
  where (a:as, hs, ts') = extract n v ts

tokIsInHS :: PpTokOrWhitespace -> HideSet -> Bool
tokIsInHS (PpTok (Identifier s)) hs = s `DS.member` hs
tokIsInHS _ _ = False

macrosAreIdentical :: Macro -> Macro -> Bool
macrosAreIdentical (ObjectMacro r1) m@(ObjectMacro r2) | replacementListsAreIdentical r1 r2 = True
macrosAreIdentical (FunctionMacro p1 v1 r1) m@(FunctionMacro p2 v2 r2) | p1 == p2 && v1 == v2 && replacementListsAreIdentical r1 r2 = True
macrosAreIdentical _ _ = False

replacementListsAreIdentical :: [PpTokOrWhitespace] -> [PpTokOrWhitespace] -> Bool
replacementListsAreIdentical as bs = foldWhitespace as == foldWhitespace bs

