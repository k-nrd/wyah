module Calculator.Repl where

import Calculator.Check (check)
import Calculator.Eval (eval)
import Calculator.Expr (Expr)
import Calculator.Parser (parseExpr)
import Calculator.Pretty (ppexpr, pptype)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import Data.Text (pack)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)

eval' :: Expr -> Expr
eval' = fromJust . eval

process :: String -> IO ()
process line = do
  case parseExpr (pack line) of
    Left err -> print err
    Right expr -> do
      case check expr of
        Left err -> print err
        Right typ -> putStrLn $ ppexpr (eval' expr) ++ " : " ++ pptype typ

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Arith> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
