{-# LANGUAGE ViewPatterns #-}
module Lexer where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Char (isDigit, isHexDigit, digitToInt)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)

newtype PhysicalSourceCharacter = PSC Char
  deriving (Show)
newtype BasicSourceCharacter = BSC Char
  deriving (Eq, Show)

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
whitespaceKind :: Char -> Maybe Whitespace
whitespaceKind ' ' = Just Horizontal
whitespaceKind '\t' = Just Horizontal
whitespaceKind '\v' = Just Vertical
whitespaceKind '\r' = Just Vertical
whitespaceKind '\n' = Just Newline
whitespaceKind _ = Nothing

isWhitespace :: Char -> Bool
isWhitespace = isJust . whitespaceKind

isBasicSourceCharacter :: Char -> Bool
isBasicSourceCharacter c
  = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || isWhitespace c || c `elem` "_{}[]#()<>%:;.?*+-/^&|~!=,\\\"'"

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
  | DRCharSequence -- r-char-sequence
  | Phase12General -- anything else

isSurrogateUCN :: Int -> Bool
isSurrogateUCN cu = cu >= 0xD800 && cu <= 0xDFFF 

isValidUCN :: Phase12Kind -> Char -> Bool
isValidUCN Phase12General ch = not (isBasicSourceCharacter ch) && ch >= '\x20' && not (ch >= '\x7F' && ch <= '\x9F')
isValidUCN _ _ = True

isValidPSC :: Phase12Kind -> Char -> Bool
isValidPSC Phase12General ch = isWhitespace ch || (ch >= '\x20' && not (ch >= '\x7F' && ch <= '\x9F'))
isValidPSC _ _ = True

phase12 :: Phase12Kind -> [PhysicalSourceCharacter] -> (Maybe BasicSourceCharacter, [PhysicalSourceCharacter])
phase12 DRCharSequence pscs = (fmap BSC c, cs)
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
    phase1 (impldef_phase1 -> (Just c, cs)) | isValidPSC kind c = (Just (BSC c), cs)
                                            | otherwise = error "physical source character is not valid outside a string literal or character literal"
    phase1 _ = (Nothing, [])
    
    phase2 :: [PhysicalSourceCharacter] -> (Maybe BasicSourceCharacter, [PhysicalSourceCharacter])
    phase2 (phase1 -> (Just (BSC '\\'), (phase1 -> (Just (BSC '\n'), cs)))) = phase2 cs
    phase2 cs = phase1 cs

data PreprocessingToken
  -- FIXME: What is a header-name?
  -- FIXME: Better representation?
  = HeaderName String
  | Identifier String
  | PpNumber String
  | CharacterLiteral String
  | UserDefinedCharacterLiteral String
  | StringLiteral String
  | UserDefinedStringLiteral String
  | PreprocessingOpOrPunc String
  | Other BasicSourceCharacter
  deriving (Show)

data Whitespace
  = Horizontal
  | Vertical
  | Newline
  deriving (Show)

data PpTokOrWhitespace
  = PpTok PreprocessingToken
  | Whitespace Whitespace
  deriving (Show)

class Monad m => LexerMonad m
instance LexerMonad Maybe

data Phase123State
  = StartOfLine
  | AfterHash
  | AfterHashInclude
  | AnywhereElse

-- [lex.operators]
preprocessingOpOrPunc = [
  "{", "}", "[", "]", "#", "##", "(", ")",
  "<:", ":>", "<%", "%>", "%:", "%:%:", ";", ":", "...",
  "new", "delete", "?", "::", ".", ".*",
  "+", "-", "*", "/", "%", "^", "&", "|", "~",
  "!", "=", "<", ">", "+=", "-=", "*=", "/=", "%=",
  "^=", "&=", "|=", "<<", ">>", ">>=", "<<=", "==", "!=",
  "<=", ">=", "&&", "||", "++", "--", ",", "->*", "->",
  "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq",
  "or", "or_eq", "xor", "xor_eq"]

-- [lex.ppnumber]
simplePpNumberSuffix = ["e+","E+","e-","E-","."]
isPpNumberDigitNoSep c = isDigit c || isIdentifierNondigit c
isPpNumberDigitSep c = isDigit c || isNondigit c

-- [lex.name]
isNondigit c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
isIdentifierNondigit c = isNondigit c || not (isBasicSourceCharacter c)

-- [lex.icon]
isOctalDigit = (`elem` ['0'..'7'])
isHexadecimalDigit = isHexDigit

-- [lex.ccon]
isSimpleEscapeSequence = (`elem` "'\"?\\abfnrtv")
csLiteralPrefix = oneOf ["", "u", "U", "L"]
isCChar c = c `notElem` "'\\\n"
isSChar c = c `notElem` "\"\\\n"
isDChar c = c `notElem` " ()\\\t\v\r\n"

-- [lex.header]
isHChar c = c `notElem` "\n>"
isQChar c = c `notElem` "\n\""

satisfies :: (Char -> Bool) -> [PhysicalSourceCharacter] -> (Maybe Char, [PhysicalSourceCharacter])
satisfies f (phase12 Phase12General -> (Just (BSC x), xs)) | f x = (Just x, xs)
satisfies _ xs = (Nothing, xs)

matchGen :: Phase12Kind -> String -> [PhysicalSourceCharacter] -> (Maybe String, [PhysicalSourceCharacter])
matchGen kind d cs = run d cs
  where
    run (d:ds) (phase12 kind -> (Just (BSC c), cs)) | c == d = run ds cs
    run (d:ds) _ = (Nothing, [])
    run [] cs = (Just d, cs)
match = matchGen Phase12General
matchRaw = matchGen DRCharSequence

oneOf :: [String] -> [PhysicalSourceCharacter] -> (Maybe String, [PhysicalSourceCharacter])
oneOf ss cs = run (reverse $ sortBy (comparing length) ss) cs
  where
    run (s:ss) (match s -> r@(Just _, _)) = r
    run (s:ss) cs = run ss cs
    run [] cs = (Nothing, cs)

upTo :: Int -> ([a] -> (Maybe b, [a])) -> [a] -> ([b], [a])
upTo 0 _ as = ([], as)
upTo n f (f -> (Just b, as)) = (b:bs, as)
  where (bs, as) = upTo (n - 1) f as
upTo _ _ as = ([], as)

many :: ([a] -> (Maybe b, [a])) -> [a] -> ([b], [a])
many f (f -> (Just b, as)) = (b:bs, as')
  where (bs, as') = many f as
many f cs = ([], cs)

many1 :: ([a] -> (Maybe b, [a])) -> [a] -> (Maybe [b], [a])
many1 f (f -> (Just b, as)) = (Just (b:bs), as')
  where (bs, as') = many f as
many1 f cs = (Nothing, cs)

phase123 :: LexerMonad m => [PhysicalSourceCharacter] -> m [PpTokOrWhitespace]
phase123 pscs = return $ run StartOfLine pscs
  where
    run _ [] = []
    run AfterHashInclude (headerName -> (Just hn, cs)) = PpTok hn:run AnywhereElse cs
    run state cs = (tok:run (newState state tok) cs')
      where (tok, cs') = ppTokOrWhitespace cs
            newState _ (Whitespace Newline) = StartOfLine
            newState StartOfLine (PpTok (PreprocessingOpOrPunc "#")) = AfterHash
            newState AfterHash (PpTok (Identifier "include")) = AfterHashInclude
            newState _ _ = AnywhereElse

    bsc = phase12 Phase12General
    rsc = phase12 DRCharSequence

    ppTokOrWhitespace (bsc -> (Just (BSC (whitespaceKind -> Just w)), cs)) = (Whitespace w, cs)
    ppTokOrWhitespace (bsc -> (Just (BSC '/'),
                       bsc -> (Just (BSC '*'),
                       cs))) = skipBlockComment cs
    ppTokOrWhitespace (bsc -> (Just (BSC '/'),
                       bsc -> (Just (BSC '/'),
                       cs))) = skipLineComment cs
    ppTokOrWhitespace cs = first PpTok $ ppToken cs

    ppToken (csLiteralPrefix -> (Just prefix,
             bsc -> (Just (BSC 'R'),
             bsc -> (Just (BSC '"'),
             dCharSequence -> (dcs,
             bsc -> (Just (BSC '('),
             rCharSequence dcs -> (rcs,
             bsc -> (Just (BSC ')'),
             matchRaw dcs -> (Just _,
             bsc -> (Just (BSC '"'),
             udSuffix -> (ud,
             cs))))))))))) = (StringLiteral (prefix ++ "R\"" ++ dcs ++ "(" ++ rcs ++ ")" ++ dcs ++ "\"" ++ ud), cs)
    ppToken (csLiteralPrefix -> (Just prefix,
             bsc -> (Just (BSC '\''),
             cCharSequence -> (Just ccs,
             bsc -> (Just (BSC '\''),
             udSuffix -> (ud,
             cs)))))) = (CharacterLiteral (prefix ++ "'" ++ ccs ++ "'" ++ ud), cs)
    ppToken (csLiteralPrefix -> (Just prefix,
             bsc -> (Just (BSC '"'),
             sCharSequence -> (scs,
             bsc -> (Just (BSC '"'),
             udSuffix -> (ud,
             cs)))))) = (StringLiteral (prefix ++ "\"" ++ scs ++ "\"" ++ ud), cs)
    ppToken (bsc -> (Just (BSC 'u'),
             bsc -> (Just (BSC '8'),
             bsc -> (Just (BSC '"'),
             sCharSequence -> (scs,
             bsc -> (Just (BSC '"'),
             udSuffix -> (ud,
             cs))))))) = (StringLiteral ("u8\"" ++ scs ++ "\"" ++ ud), cs)
    ppToken (bsc -> (Just (BSC c@(isDigit -> True)), cs)) = ppNumber [c] cs
    ppToken (bsc -> (Just (BSC '.'), bsc -> (Just (BSC c@(isDigit -> True)), cs))) = ppNumber ['.', c] cs
    ppToken (oneOf preprocessingOpOrPunc -> (Just s, cs)) = (PreprocessingOpOrPunc s, cs)
    ppToken (identifier -> (Just s, cs)) = (Identifier s, cs)
    ppToken (bsc -> (Just (BSC '\''), cs)) = error "unmatched ' character"
    ppToken (bsc -> (Just (BSC '"'), cs)) = error "unmatched \" character"
    ppToken (bsc -> (Just c, cs)) = (Other c, cs)

    -- [lex.comment]
    skipBlockComment (bsc -> (Just (BSC '*'),
                      bsc -> (Just (BSC '/'),
		      cs))) = (Whitespace Horizontal, cs)
    skipBlockComment (c:cs) = skipBlockComment cs
    skipBlockComment [] = error "unclosed /* comment"
    skipLineComment cs@(bsc -> (Just (BSC '\n'), _)) = (Whitespace Horizontal, cs)
    skipLineComment (oneOf ["\r", "\v"] -> (Just _,
                     many (satisfies isWhitespace) -> (_,
                     cs@(bsc -> (Just (BSC '\n'), _))))) = (Whitespace Horizontal, cs)
    skipLineComment (oneOf ["\r", "\v"] -> (Just _, _)) = error "vertical whitespace in // comment not followed by newline"
    skipLineComment (c:cs) = skipLineComment cs
    skipLineComment [] = error "no newline after // comment"

    ppNumber ns (oneOf simplePpNumberSuffix -> (Just s, cs)) = ppNumber (ns ++ s) cs
    ppNumber ns (bsc -> (Just (BSC c@(isPpNumberDigitNoSep -> True)), cs)) = ppNumber (ns ++ [c]) cs
    ppNumber ns (bsc -> (Just (BSC '\''), bsc -> (Just (BSC c@(isPpNumberDigitSep -> True)), cs))) = ppNumber (ns ++ ['\'', c]) cs
    ppNumber ns cs = (PpNumber ns, cs)

    identifierNondigit (csc -> (Just (BSC d@(isIdentifierNondigit -> True)), cs)) = (Just d, cs)
    identifierNondigit _ = (Nothing, [])

    identifierBody (identifierNondigit -> r@(Just _, _)) = r
    identifierBody (digit -> r@(Just _, _)) = r
    identifierBody _ = (Nothing, [])

    identifier (identifierNondigit -> (Just i, many identifierBody -> (is, cs))) = (Just (i:is), cs)
    identifier _ = (Nothing, [])

    udSuffix (identifier -> (Just id, cs)) = (id, cs)
    udSuffix cs = ([], cs)

    csc = phase12 CSCharSequence
    digit (csc -> (Just (BSC d@(isDigit -> True)), cs)) = (Just d, cs)
    digit _ = (Nothing, [])
    octalDigit (csc -> (Just (BSC d@(isOctalDigit -> True)), cs)) = (Just d, cs)
    octalDigit _ = (Nothing, [])
    hexadecimalDigit (csc -> (Just (BSC d@(isHexadecimalDigit -> True)), cs)) = (Just d, cs)
    hexadecimalDigit _ = (Nothing, [])

    escapeSequence (csc -> (Just (BSC '\\'),
                    csc -> (Just (BSC c@(isSimpleEscapeSequence -> True)),
                    cs))) = (Just ['\\', c], cs)
    escapeSequence (csc -> (Just (BSC '\\'),
                    upTo 3 octalDigit -> (os@(_:_),
                    cs))) = (Just ("\\" ++ os), cs)
    escapeSequence (csc -> (Just (BSC '\\'),
                    oneOf ["x", "X"] -> (Just x,
                    many hexadecimalDigit -> (xs@(_:_),
                    cs)))) = (Just ("\\" ++ x ++ xs), cs)
    escapeSequence cs = (Nothing, [])

    cChar (csc -> (Just (BSC c@(isCChar -> True)), cs)) = (Just [c], cs)
    cChar (escapeSequence -> (Just es, cs)) = (Just es, cs)
    cChar _ = (Nothing, [])
    cCharSequence = (fmap.first.fmap) concat $ many1 cChar

    sChar (csc -> (Just (BSC c@(isSChar -> True)), cs)) = (Just [c], cs)
    sChar (escapeSequence -> (Just es, cs)) = (Just es, cs)
    sChar _ = (Nothing, [])
    sCharSequence = (fmap.first) concat $ many sChar

    dChar (rsc -> (Just (BSC c@(isDChar -> True)), cs)) = (Just c, cs)
    dChar _ = (Nothing, [])
    dCharSequence = many dChar

    rChar dcs (matchRaw (")" ++ dcs ++ "\"") -> (Just _, _)) = (Nothing, [])
    rChar dcs (rsc -> (Just (BSC c), cs)) = (Just c, cs)
    rChar dcs _ = (Nothing, [])
    rCharSequence dcs = many (rChar dcs)

    hChar (bsc -> (Just (BSC c@(isHChar -> True)), cs)) = (Just c, cs)
    hChar _ = (Nothing, [])
    hCharSequence = many1 hChar

    qChar (bsc -> (Just (BSC c@(isQChar -> True)), cs)) = (Just c, cs)
    qChar _ = (Nothing, [])
    qCharSequence = many1 qChar

    headerName (bsc -> (Just (BSC '<'),
                hCharSequence -> (Just hcs,
                bsc -> (Just (BSC '>'),
                cs)))) = (Just $ HeaderName ("<" ++ hcs ++ ">"), cs)
    headerName (bsc -> (Just (BSC '"'),
                qCharSequence -> (Just qcs,
                bsc -> (Just (BSC '"'),
                cs)))) = (Just $ HeaderName ("\"" ++ qcs ++ "\""), cs)
    headerName _ = (Nothing, [])
