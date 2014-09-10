{-# LANGUAGE ViewPatterns #-}
module Preprocessor where

import Lexer (BasicSourceCharacter(..),
              PpTokOrWhitespace(..),
              PreprocessingToken(..))

containsNewline :: PpTokOrWhitespace -> Bool
containsNewline (Whitespace ws) = BSC '\n' `elem` ws
containsNewline _ = False

newtype Group = Group [GroupPart] deriving (Show)
data GroupPart
  = IfSection IfGroup [ElifGroup] Group EndifLine
  -- non-directives are modeled as control-lines.
  | ControlLine [PpTokOrWhitespace]
  | TextLine [PpTokOrWhitespace]
  deriving (Show)
data IfGroup
  = If [PpTokOrWhitespace] Group
  | Ifdef [PpTokOrWhitespace] Group
  | Ifndef [PpTokOrWhitespace] Group
  deriving (Show)
data ElifGroup
  = Elif [PpTokOrWhitespace] Group
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
optionalWhitespace (t@(Whitespace _):ts) | not (containsNewline t) = ts
optionalWhitespace r = r

-- Identify and extract a directive.
directive :: [PpTokOrWhitespace] -> Maybe (String, [PpTokOrWhitespace], [PpTokOrWhitespace])
directive (PpTok (PreprocessingOpOrPunc "#") :
           (optionalWhitespace -> (PpTok (Identifier directive):ts))) = Just (directive, line, rest)
  where
    (line, rest) = splitAtNewline ts
directive _ = Nothing

makePpFile :: Monad m => [PpTokOrWhitespace] -> m Group
makePpFile toks = do
    (g, toks') <- makeGroup toks
    case toks' of
      [] -> return g
      _ -> error "#else/#elif/#endif with no matching #if"
  where
    makeGroupPart [] = endGroup []
    makeGroupPart (directive -> Just ("if", line, rest)) = makeIfSection (If line) rest
    makeGroupPart (directive -> Just ("ifdef", line, rest)) = makeIfSection (Ifdef line) rest
    makeGroupPart (directive -> Just ("ifndef", line, rest)) = makeIfSection (Ifndef line) rest
    makeGroupPart toks@(directive -> Just ("elif", _, _)) = endGroup toks
    makeGroupPart toks@(directive -> Just ("else", _, _)) = endGroup toks
    makeGroupPart toks@(directive -> Just ("endif", _, _)) = endGroup toks
    makeGroupPart toks@(directive -> Just _) = finishGroupPart (ControlLine line) rest
      where (line, rest) = splitAtNewline toks
    makeGroupPart toks = finishGroupPart (TextLine line) rest
      where (line, rest) = splitAtNewline toks

    makeGroup = makeGroupFrom []
      where
        makeGroupFrom gps toks = do
          (mp, toks') <- makeGroupPart toks
          case mp of
            Just p -> makeGroupFrom (p:gps) toks'
    	    Nothing -> return (Group (reverse gps), toks')

    finishGroupPart part toks = return (Just part, toks)
    endGroup toks = return (Nothing, toks)

    makeIfSection ifGroupHead ts = do
      (ifg, ts') <- makeGroup ts
      (elifs, ts'') <- makeElifGroups [] ts'
      (elseg, ts''') <- makeElseGroup ts''
      (endif, ts'''') <- makeEndifLine ts'''
      finishGroupPart (IfSection (ifGroupHead ifg) elifs elseg endif) ts''''
    makeElifGroups elifs (directive -> Just ("elif", line, rest)) = do
      (g, ts) <- makeGroup rest
      makeElifGroups (Elif line g:elifs) ts
    makeElifGroups elifs ts = return (reverse elifs, ts)
    makeElseGroup (directive -> Just ("else", line, rest)) = makeGroup rest
    makeElseGroup toks = return (Group [], toks)
    makeEndifLine (toks@(directive -> Just ("endif", line, rest))) = return (Endif line, rest)
    makeEndifLine _ = error "missing #endif"
