module Kernel where

import Control.Monad.Except (runExcept)
import Lambda (Expr, parseExpression)
import System.IO (hFlush, stdout)
import Text.Parsec (runParser)
import Typing (principlePair)

data Var = Var String Expr

newtype Scope = Scope [Var]

processLine :: String -> String
processLine input = case runParser parseExpression "" "" input of
  Left error -> "Parse Error: " ++ show error
  Right expr -> case runExcept (principlePair expr) of
    Left err -> "Typecheck Error: " ++ err
    Right (env, tp) -> "(" ++ show env ++ ")" ++ " => " ++ show tp

routine :: IO ()
routine = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  putStrLn $ processLine input
