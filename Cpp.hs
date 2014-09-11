module Main where

import Lexer
import Preprocessor

import qualified Data.Map as DM
import Control.Monad.State

type MacroScope = DM.Map String Macro

predefinedMacros = DM.fromList [
  ("__cplusplus", ppNumber "201402L"),
  ("__FILE__", ppString "\"foo.cpp\""),
  ("__LINE__", ppNumber "123"),
  ("A", ObjectMacro [])]
  where ppNumber x = ObjectMacro [PpTok $ PpNumber x]
        ppString x = ObjectMacro [PpTok $ StringLiteral x]

newtype PP x = PP { runPP :: State MacroScope x }

instance Monad PP where
  return = PP . return
  a >>= f = PP $ runPP a >>= (\x -> runPP $ f x)

instance MonadPreprocessor PP where
  includeFile n = return (Group [TextLine [PpTok $ StringLiteral n]])
  --macroDefinition "__cplusplus" = return . Just $ ObjectMacro [PpTok $ PpNumber "201402L"]
  --macroDefinition "__FILE__" = return . Just $ ObjectMacro [PpTok $ StringLiteral "\"foo.cpp\""]
  --macroDefinition "__LINE__" = return . Just $ ObjectMacro [PpTok $ PpNumber "123"]
  --macroDefinition "A" = return . Just $ ObjectMacro []
  macroDefinition name = PP $ do
    scope <- get
    return $ DM.lookup name scope
  defineMacro name macro = PP $ do
    scope <- get
    put $ DM.insert name macro scope
  undefineMacro name = PP $ do
    scope <- get
    put $ DM.delete name scope

main = do
  s <- getContents
  let ppToks = flip evalState predefinedMacros . runPP . extractTokens . concatTextLines . makePpFile . phase123 $ map PSC s
  putStrLn $ concatMap ((++ "\n") . show) ppToks
