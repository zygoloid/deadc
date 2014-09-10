{-# LANGUAGE ViewPatterns #-}
module Preprocessor where

import Lexer (BasicSourceCharacter(..),
              PpTokOrWhitespace(..),
              PreprocessingToken(..),
              Whitespace(..))

import Control.Arrow (first)
import Control.Monad (liftM, liftM2)

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
directive (PpTok (PreprocessingOpOrPunc "#") : ts) = Just ("", line, rest)
  where
    (line, rest) = splitAtNewline ts
directive _ = Nothing

makePpFile :: [PpTokOrWhitespace] -> Group
makePpFile toks = case makeGroup toks of
    (g, []) -> g
    _ -> error "#else/#elif/#endif with no matching #if"
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
      where run (makeGroupPart -> (Just p, toks)) = first (p:) (run toks)
            run toks = ([], toks)

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
  includeFile :: Bool -> String -> m Group

-- FIXME: The standard doesn't say to do this, but it's implied.
concatTextLines :: Group -> Group
concatTextLines (Group gps) = Group (run gps)
  where
    run :: [GroupPart] -> [GroupPart]
    run (TextLine ts1:TextLine ts2:gps) = run (TextLine (ts1 ++ ts2):gps)
    run (IfSection ig g eil:gps) = IfSection (map concatInGroup ig) (concatTextLines g) eil:run gps
      where concatInGroup (IfGroup c g) = IfGroup c (concatTextLines g)
    run (gp:gps) = gp:run gps
    run [] = []

cleanDirective :: [PpTokOrWhitespace] -> [PpTokOrWhitespace]
cleanDirective = map clean
  where clean (Whitespace Vertical) = error "vertical whitespace within directive"
        clean x = x

extractTokens :: MonadPreprocessor m => Group -> m [PpTokOrWhitespace]
extractTokens (Group gps) = liftM concat $ mapM extractTokensFromLine gps

extractTokensFromLine (IfSection ifgs elseg eol) = do
  g <- findEnabledGroup ifgs elseg
  extractTokens g
extractTokensFromLine (ControlLine d (cleanDirective -> toks)) = handleDirective d toks
extractTokensFromLine (TextLine ts) = expandMacros ts

handleDirective :: MonadPreprocessor m => String -> [PpTokOrWhitespace] -> m [PpTokOrWhitespace]
handleDirective d toks = return []
-- FIXME: header-name or expand macros
--handleDirective "include" toks = do
--  Group gps' <- includeFile toks
--  extractTokens (gps' ++ gps)

findEnabledGroup :: MonadPreprocessor m => [IfGroup] -> Group -> m Group
findEnabledGroup (IfGroup c g:igs) elseg = do
  b <- evaluateCondition c
  if b then return g else findEnabledGroup igs elseg
findEnabledGroup [] elseg = return elseg

ifdefMacroName :: [PpTokOrWhitespace] -> Maybe String
ifdefMacroName (cleanDirective -> optionalWhitespace ->
                PpTok (Identifier m):(optionalWhitespace -> [])) = Just m
ifdefMacroName _ = Nothing

evaluateCondition :: MonadPreprocessor m => IfCond -> m Bool
evaluateCondition (If (cleanDirective -> toks)) = do
  toks' <- expandMacros toks
  -- FIXME: ...
  return True
evaluateCondition (Ifdef (ifdefMacroName -> Just m)) = do
  return True -- FIXME
evaluateCondition (Ifndef (ifdefMacroName -> Just m)) = do
  return True -- FIXME

expandMacros :: MonadPreprocessor m => [PpTokOrWhitespace] -> m [PpTokOrWhitespace]
expandMacros = return
