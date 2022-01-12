module Language.Repl where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (pack)
import Language.Eval (eval)
import Language.Lexer (parseExpr)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)

process :: String -> IO ()
process line = do
  case parseExpr (pack line) of
    Left err -> print err
    Right expr -> print $ eval expr

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Arith> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
