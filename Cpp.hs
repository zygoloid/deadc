module Main where

import Lexer
import Preprocessor

data Id x = Id { runId :: x }
instance Monad Id where
  return = Id
  a >>= f = f (runId a)
instance MonadPreprocessor Id where
  includeFile = error "can't include files"
  macroDefinition "__cplusplus" = return (Just Macro)
  macroDefinition "A" = return (Just Macro)
  macroDefinition _ = return Nothing

main = do
  s <- getContents
  let Id ppToks = extractTokens . concatTextLines . makePpFile . phase123 $ map PSC s
  putStrLn $ concatMap ((++ "\n") . show) ppToks
