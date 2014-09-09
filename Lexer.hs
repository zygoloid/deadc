{-# LANGUAGE ViewPatterns #-}
module Lexer where

import Data.Char (isHexDigit, digitToInt)

newtype PhysicalSourceCharacter = PSC Char
  deriving (Show)
newtype BasicSourceCharacter = BSC Char
  deriving (Show)

impldef_phase1 :: [PhysicalSourceCharacter] -> (Maybe Char, [PhysicalSourceCharacter])
impldef_phase1 (PSC c:cs) = (Just c, cs)
impldef_phase1 [] = (Nothing, [])

-- [lex.trigraph]
trigraph :: Char -> Maybe Char
trigraph '=' = Just '#'
trigraph '/' = Just '\\'
trigraph '\'' = Just '^'
trigraph '(' = Just '['
trigraph ')' = Just ']'
trigraph '!' = Just '|'
trigraph '<' = Just '{'
trigraph '>' = Just '}'
trigraph '-' = Just '~'

-- [lex.charset]
isBasicSourceCharacter :: Char -> Bool
isBasicSourceCharacter c
  = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c `elem` "_{}[]#()<>%:;.?*+-/^&|~!=,\\\"'"

repeated :: Int -> ([a] -> (Maybe b, [a])) -> [a] -> ([b], [a])
repeated 0 f as = ([], as)
repeated n f as = case b of
    Just b' -> (b':bs, as'')
    Nothing -> ([], as'')
  where (b, as') = f as
        (bs, as'') = repeated (n-1) f as'

hexDigit :: String -> (Maybe Int, String)
hexDigit (c:cs) | isHexDigit c = (Just (digitToInt c), cs)
hexDigit cs = (Nothing, cs)

data Phase12Kind
  = CSCharSequence -- s-char-sequence or c-char-sequence
  | RCharSequence -- r-char-sequence
  | Phase12General -- anything else

isSurrogateUCN :: Int -> Bool
isSurrogateUCN cu = cu >= 0xD800 && cu <= 0xDFFF 

isValidUCN :: Phase12Kind -> Char -> Bool
isValidUCN Phase12General ch = not (isBasicSourceCharacter ch) && ch >= '\x20' && not (ch >= '\x7F' && ch <= '\x9F')
isValidUCN _ _ = True

phase12 :: Phase12Kind -> [PhysicalSourceCharacter] -> (Maybe BasicSourceCharacter, [PhysicalSourceCharacter])
phase12 RCharSequence pscs = (fmap BSC c, cs)
  where (c, cs) = impldef_phase1 pscs
phase12 kind pscs = phase2 pscs
  where
    hexDigitsToBSC :: Monad m => [Int] -> m BasicSourceCharacter
    hexDigitsToBSC is | isSurrogateUCN cu = error "universal-character-name shall not name a surrogate code point"
                      | cu > 0x10ffff = error "universal-character-name shall name an ISO/IEC 10646 character"
                      | not (isValidUCN kind cp) = error "universal-character-name specifies character that is not valid outside a string or character literal"
                      | otherwise = return (BSC cp)
      where cu = foldl (\n i -> n * 16 + i) 0 is
            cp = toEnum cu :: Char
    
    phase1 :: [PhysicalSourceCharacter] -> (Maybe BasicSourceCharacter, [PhysicalSourceCharacter])
    phase1 (repeated 3 impldef_phase1 -> ('?':'?':[trigraph -> Just t], cs)) = (Just (BSC t), cs)
    -- phase1 (impldef_phase1 -> ('?', impldef_phase1 -> ('?', impldef_phase1 -> (trigraph -> Just t, cs)))) = (Just t, cs)
    -- Internal encoding for extended characters is as Unicode code points.
    phase1 (repeated 6 impldef_phase1 -> ('\\':'u':(repeated 4 hexDigit -> ([a,b,c,d], [])), cs)) = (hexDigitsToBSC [a,b,c,d], cs)
    phase1 (repeated 10 impldef_phase1 -> ('\\':'U':'0':'0':(repeated 6 hexDigit -> ([a,b,c,d,e,f], [])), cs)) = (hexDigitsToBSC [a,b,c,d,e,f], cs)
    phase1 (impldef_phase1 -> (Just c, cs)) = (Just (BSC c), cs)
    phase1 _ = (Nothing, [])
    
    phase2 :: [PhysicalSourceCharacter] -> (Maybe BasicSourceCharacter, [PhysicalSourceCharacter])
    phase2 (phase1 -> (Just (BSC '\\'), (phase1 -> (Just (BSC '\n'), cs)))) = phase2 cs
    phase2 cs = phase1 cs

--data PreprocessingToken
--  = ...
--
--data PpTokOrWhitespace
-- = PpTok PreprocessingToken
-- = Whitespace [BasicSourceCharacter]
--
--class Monad m => LexerMonad m
--
--phase123 :: LexerMonad m => [PhysicalSourceCharacter] -> m [PreprocessingToken]
--phase123 
