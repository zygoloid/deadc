module Main where

import Lexer
import Macros
import Preprocessor

import Control.Monad.State
import qualified Data.Map as DM

predefinedMacros = DM.fromList [
  ("__cplusplus", ppNumber "201402L"),
  ("__FILE__", ppString "\"foo.cpp\""),
  ("__LINE__", ppNumber "123"),
  ("STR", FunctionMacro ["x"] False [ppIdent "STR2", ppPunc "(", ppIdent "x", ppPunc ")"]),
  ("STR2", FunctionMacro ["x"] False [ppPunc "#", ppIdent "x"]),
  ("PASTE", FunctionMacro ["x", "y"] False [ppIdent "x", ppPunc "##", ppIdent "y"]),
  ("PASTEL", FunctionMacro ["x"] False [ppIdent "x", ppPunc "##", ppIdent "y"]),
  ("PASTER", FunctionMacro ["y"] False [ppIdent "x", ppPunc "##", ppIdent "y"]),
  ("A", ObjectMacro [])]
  where ppNumber x = ObjectMacro [PpTok $ PpNumber x]
        ppString x = ObjectMacro [PpTok $ StringLiteral x]
        ppIdent x = PpTok $ Identifier x
        ppPunc x = PpTok $ PreprocessingOpOrPunc x

newtype PP x = PP { runPP :: State MacroScope x }

instance Monad PP where
  return = PP . return
  a >>= f = PP $ runPP a >>= (\x -> runPP $ f $! x)

instance MonadPreprocessor PP where
  includeFile n = return (Group [TextLine [PpTok $ StringLiteral n]])
  getMacroScope = PP get
  putMacroScope = PP . put

main = do
  s <- getContents
  let ppToks = flip evalState predefinedMacros . runPP . extractTokens . concatTextLines . makePpFile . phase123 $ map PSC s
  putStrLn $ concatMap ((++ "\n") . show) ppToks
