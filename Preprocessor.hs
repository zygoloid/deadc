{-# LANGUAGE ViewPatterns #-}
module Preprocessor where

import Lexer (BasicSourceCharacter(..),
              PpTokOrWhitespace(..),
              PreprocessingToken(..),
              Whitespace(..))
import Macros

import Control.Arrow (first)
import Control.Monad (liftM, liftM2)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as DM

containsNewline :: PpTokOrWhitespace -> Bool
containsNewline (Whitespace Newline) = True
containsNewline _ = False

newtype Group = Group [GroupPart] deriving (Show)
data GroupPart
  = IfSection [IfGroup] Group EndifLine
  -- non-directives are modeled as control-lines.
  | ControlLine String [PpTokOrWhitespace]
  | TextLine [PpTokOrWhitespace]
  deriving (Show)
data IfGroup = IfGroup IfCond Group deriving (Show)
data IfCond
  = If [PpTokOrWhitespace]
  | Ifdef [PpTokOrWhitespace]
  | Ifndef [PpTokOrWhitespace]
  deriving (Show)
data EndifLine
  = Endif [PpTokOrWhitespace]
  deriving (Show)

splitAtNewline :: [PpTokOrWhitespace] -> ([PpTokOrWhitespace], [PpTokOrWhitespace])
splitAtNewline (t:ts) | containsNewline t = ([t], ts)
                      | otherwise = (t:as, bs)
  where (as, bs) = splitAtNewline ts
splitAtNewline [] = ([], [])

-- Skip optional whitespace not containing a newline.
optionalWhitespace :: [PpTokOrWhitespace] -> [PpTokOrWhitespace]
optionalWhitespace r@(Whitespace Newline:_) = r
optionalWhitespace (Whitespace _:ts) = optionalWhitespace ts
optionalWhitespace r = r

-- Identify and extract a directive.
directive :: [PpTokOrWhitespace] -> Maybe (String, [PpTokOrWhitespace], [PpTokOrWhitespace])
directive (optionalWhitespace -> PpTok (PreprocessingOpOrPunc "#") :
           (optionalWhitespace -> (PpTok (Identifier directive):ts))) = Just (directive, line, rest)
  where
    (line, rest) = splitAtNewline ts
-- Null directive and some forms of non-directive
directive (optionalWhitespace -> PpTok (PreprocessingOpOrPunc "#") : ts) = Just ("", line, rest)
  where
    (line, rest) = splitAtNewline ts
directive _ = Nothing

makePpFile :: [PpTokOrWhitespace] -> Group
makePpFile toks = makeTopLevelGroup toks
  where
    makeGroupPart [] = endGroup []
    makeGroupPart (directive -> Just ("if", line, rest)) = makeIfSection (If line) rest
    makeGroupPart (directive -> Just ("ifdef", line, rest)) = makeIfSection (Ifdef line) rest
    makeGroupPart (directive -> Just ("ifndef", line, rest)) = makeIfSection (Ifndef line) rest
    makeGroupPart toks@(directive -> Just ("elif", _, _)) = endGroup toks
    makeGroupPart toks@(directive -> Just ("else", _, _)) = endGroup toks
    makeGroupPart toks@(directive -> Just ("endif", _, _)) = endGroup toks
    makeGroupPart (directive -> Just (d, line, rest)) = finishGroupPart (ControlLine d line) rest
    makeGroupPart toks = finishGroupPart (TextLine line) rest
      where (line, rest) = splitAtNewline toks

    finishGroupPart part toks = (Just part, toks)
    endGroup toks = (Nothing, toks)

    makeGroup = first Group . run
      where run (makeGroupPart -> (Just p, toks)) = (p:ps, ts)
              where (ps, ts) = run toks
            run toks = ([], toks)

    makeTopLevelGroup = Group . run
      where run [] = []
            run ts = case makeGroupPart ts of
              (Just p, ts') -> p:run ts'
              _ -> error "#else/#elif/#endif with no matching #if"

    makeIfSection ifCond ts = result
      where
        (ifg, ts') = makeGroup ts
        (elifs, ts'') = makeElifGroups [] ts'
        (elseg, ts''') = makeElseGroup ts''
        (endif, ts'''') = makeEndifLine ts'''
        result = finishGroupPart (IfSection (IfGroup ifCond ifg:elifs) elseg endif) ts''''
    makeElifGroups elifs (directive -> Just ("elif", line, rest)) = makeElifGroups (IfGroup (If line) g:elifs) ts
      where (g, ts) = makeGroup rest
    makeElifGroups elifs ts = (reverse elifs, ts)
    makeElseGroup (directive -> Just ("else", line, rest)) = makeGroup rest
    makeElseGroup toks = (Group [], toks)
    makeEndifLine (toks@(directive -> Just ("endif", line, rest))) = (Endif line, rest)
    makeEndifLine _ = error "missing #endif"

class Monad m => MonadPreprocessor m where
  includeFile :: String -> m Group
  getMacroScope :: m MacroScope
  putMacroScope :: MacroScope -> m ()

macroDefinition :: MonadPreprocessor m => String -> m (Maybe Macro)
macroDefinition name = do
  scope <- getMacroScope
  return $ DM.lookup name scope

defineMacro :: MonadPreprocessor m => String -> Macro -> m ()
defineMacro name macro = do
    scope <- getMacroScope
    case DM.lookup name scope of
      Just old | not (macrosAreIdentical old macro) -> error "macro redefined with different body"
      _ -> putMacroScope $ DM.insert name macro scope

undefineMacro :: MonadPreprocessor m => String -> m ()
undefineMacro name = do
  scope <- getMacroScope
  putMacroScope (DM.delete name scope)

-- FIXME: The standard doesn't say to do this, but it's implied.
concatTextLines :: Group -> Group
concatTextLines (Group gps) = Group (run gps)
  where
    run :: [GroupPart] -> [GroupPart]
    run (TextLine ts:gps) = TextLine (ts ++ rest):run gps'
      where extract (TextLine ts:gps) = first (ts ++) $ extract gps
            extract gps = ([], gps)
            (rest, gps') = extract gps
    run (IfSection ig g eil:gps) = IfSection (map concatInGroup ig) (concatTextLines g) eil:run gps
      where concatInGroup (IfGroup c g) = IfGroup c (concatTextLines g)
    run (gp:gps) = gp:run gps
    run [] = []

-- FIXME: We also need to clean whitespace between the # and the macro name, which is discarded before we get here!
-- Maybe just flatten all vertical whitespace to a single space in phase 3, then discard this check.
cleanDirective :: [PpTokOrWhitespace] -> [PpTokOrWhitespace]
cleanDirective = map clean
  where clean (Whitespace Vertical) = error "vertical whitespace within directive"
        clean x = x

extractTokens :: MonadPreprocessor m => Group -> m [PpTokOrWhitespace]
extractTokens (Group gps) = liftM concat $ mapM extractTokensFromLine gps

extractTokensFromLine (IfSection ifgs elseg eol) = do
  g <- findEnabledGroup ifgs elseg
  extractTokens g
extractTokensFromLine (ControlLine d (cleanDirective -> toks)) = handleDirective d (optionalWhitespace toks)
extractTokensFromLine (TextLine ts) = expand ts

handleDirective :: MonadPreprocessor m => String -> [PpTokOrWhitespace] -> m [PpTokOrWhitespace]
handleDirective "include" (PpTok (HeaderName n):(isEndOfDirective -> True)) = do
  g <- includeFile n
  extractTokens g
handleDirective "include" toks = do
  toks' <- expand toks
  g <- includeFile (impldef_stringizeForInclude toks')
  extractTokens g
handleDirective "define" (PpTok (Identifier m):PpTok (PreprocessingOpOrPunc "("):ts) = defineMacro m (parseFunctionMacro ts) >> return []
handleDirective "define" (PpTok (Identifier m):Whitespace _:ts) = defineMacro m (ObjectMacro (strip ts)) >> return []
handleDirective "define" (PpTok (Identifier m):ts) = error "macro name in #define directive must be followed by ( or whitespace"
handleDirective "define" _ = error "expected macro name in #define directive"
handleDirective "undef" (PpTok (Identifier m):(isEndOfDirective -> True)) = undefineMacro m >> return []
handleDirective "undef" _ = error "expected macro name in #undef directive"
handleDirective d toks = return []

parseFunctionMacro :: [PpTokOrWhitespace] -> Macro
parseFunctionMacro (optionalWhitespace -> PpTok (PreprocessingOpOrPunc ")"):ts) = FunctionMacro [] False (strip ts)
parseFunctionMacro (optionalWhitespace -> ts) = FunctionMacro ps v (strip ts')
  where (ps, v, ts') = params ts
        params (Whitespace _:ts) = params ts
        -- FIXME: not strict enough to guarantee this appears
        params (PpTok (Identifier p):ts) | p `elem` ps = error "duplicate macro parameter name"
                                         | otherwise = (p:ps, v, ts')
          where (ps, v, ts') = delim ts
        params (PpTok (PreprocessingOpOrPunc "..."):PpTok (PreprocessingOpOrPunc ")"):ts) = ([], True, ts)
        params _ = error "expected identifier or ... in macro parameter list"
        delim (Whitespace _:ts) = delim ts
        delim (PpTok (PreprocessingOpOrPunc ","):ts) = params ts
        delim (PpTok (PreprocessingOpOrPunc ")"):ts) = ([], False, ts)
        delim _ = error "expected , or ) after macro parameter name"

findEnabledGroup :: MonadPreprocessor m => [IfGroup] -> Group -> m Group
findEnabledGroup (IfGroup c g:igs) elseg = do
  b <- evaluateCondition c
  if b then return g else findEnabledGroup igs elseg
findEnabledGroup [] elseg = return elseg

strip :: [PpTokOrWhitespace] -> [PpTokOrWhitespace]
strip = stripL . reverse . stripL . reverse
  where stripL (Whitespace _:ts) = stripL ts
        stripL ts = ts

isEndOfDirective :: [PpTokOrWhitespace] -> Bool
isEndOfDirective (optionalWhitespace -> [Whitespace Newline]) = True
isEndOfDirective _ = False

ifdefMacroName :: [PpTokOrWhitespace] -> Maybe String
ifdefMacroName (cleanDirective -> optionalWhitespace ->
                PpTok (Identifier m):(isEndOfDirective -> True)) = Just m
ifdefMacroName _ = Nothing

evaluateCondition :: MonadPreprocessor m => IfCond -> m Bool
evaluateCondition (If (cleanDirective -> toks)) = do
  toks' <- expand toks
  -- FIXME: substitute 1 for 'true', 0 for other identifiers, evaluate as expression
  return True
evaluateCondition (Ifdef (ifdefMacroName -> Just m)) = do
  md <- macroDefinition m
  return $ isJust md
evaluateCondition (Ifndef (ifdefMacroName -> Just m)) = do
  md <- macroDefinition m
  return $ isNothing md
evaluateCondition _ = error "malformed #ifdef / #ifndef condition"

expand :: MonadPreprocessor m => [PpTokOrWhitespace] -> m [PpTokOrWhitespace]
expand toks = do
  scope <- getMacroScope
  return $ replaceMacros scope toks

impldef_stringizeForInclude :: [PpTokOrWhitespace] -> String
impldef_stringizeForInclude ts = show ts
