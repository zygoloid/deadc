module Main where

import Lexer
import Preprocessor

data Id x = Id { runId :: x }
instance Monad Id where
  return = Id
  a >>= f = f (runId a)
instance MonadPreprocessor Id where
  includeFile n = return (Group [TextLine [PpTok $ StringLiteral n]])
  macroDefinition "__cplusplus" = return . Just $ ObjectMacro [PpTok $ PpNumber "201402L"]
  macroDefinition "__FILE__" = return . Just $ ObjectMacro [PpTok $ StringLiteral "\"foo.cpp\""]
  macroDefinition "__LINE__" = return . Just $ ObjectMacro [PpTok $ PpNumber "123"]
  macroDefinition "A" = return . Just $ ObjectMacro []
  macroDefinition _ = return Nothing

main = do
  s <- getContents
  let Id ppToks = extractTokens . concatTextLines . makePpFile . phase123 $ map PSC s
  putStrLn $ concatMap ((++ "\n") . show) ppToks
