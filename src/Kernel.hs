{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Kernel where

import Control.Monad.State
  ( MonadState (get, state),
    MonadTrans (lift),
    State,
    StateT,
  )
import Data.HashMap.Lazy (HashMap, empty, lookup)
import Lambda (Expr, parseExpression, parseIdentifier)
import System.IO (hFlush, stdout)
import Text.Parsec
  ( Parsec,
    char,
    runParser,
    spaces,
    string,
    (<|>),
  )
import Typing (Type, principlePair)

type Scope = HashMap String Expr

empmtyScope :: Scope
empmtyScope = empty

data Input
  = Assign String Expr
  | Evaluate Expr
  | Typeof Expr
  | QuitI

data Output
  = Result Expr
  | ExprType Type
  | ParsingError String
  | TypecheckError String
  | Message String
  | QuitO

processLine :: String -> StateT Scope IO () -> StateT Scope IO Output
processLine line scope = do
  case runParser parseInput "" "" line of
    Left error -> return $ ParsingError $ show error
    Right (Assign name expr) -> return $ Message $ "Assignation: " ++ name ++ " = " ++ show expr
    Right (Evaluate expr) -> return $ Message $ "Evaluate: " ++ show expr
    Right (Typeof expr) -> return $ Message $ "Typeof: " ++ show expr
    Right QuitI -> return $ QuitO

instance Show Output where
  show (Result expr) = show expr
  show (ExprType t) = show t
  show (ParsingError e) = "ParsingError: " ++ e
  show (TypecheckError e) = "TypecheckingError: " ++ e
  show (Message m) = m

-- processLine :: String -> String
-- processLine line =
--   case runParser parseExpression "" "" line of
--     Left error -> "Parsing Error: " ++ show error
--     Right expr -> case runExcept (principlePair expr) of
--       Left err -> "Typechecking Error: " ++ err
--       Right (env, tp) -> "(" ++ show env ++ ")" ++ " => " ++ show tp

routine :: StateT Scope IO ()
-- ^ main routine iteration
routine = do
  lift $ putStr "λ> "
  lift $ hFlush stdout
  input <- lift getLine
  scope <- get
  res <- processLine input $ state (\x -> ((), scope))

  case res of
    QuitO -> return ()
    nonQuit -> (lift $ putStrLn $ show nonQuit) >> routine

-- Result e -> (lift $ putStrLn $ show e) >> routine
-- ExprType t -> undefined
-- ParsingError m -> undefined
-- TypecheckError m -> undefined
-- Message m -> undefined

-- command parsing

parseCommandExpr :: Char -> (Expr -> Input) -> Parsec String String Input
parseCommandExpr ch inputMaker = do
  char ch >> spaces
  e <- parseExpression
  return $ inputMaker e

parseCommand :: Char -> Input -> Parsec String String Input
parseCommand ch res = do
  char ch >> spaces
  return res

parseAssign :: Parsec String String Input
parseAssign = do
  exprName <- parseIdentifier
  spaces >> char '=' >> spaces
  expr <- parseExpression
  return $ Assign exprName expr

parseInput :: Parsec String String Input
parseInput =
  parseAssign
    <|> ( char ':' >> spaces
            >> ( parseCommandExpr 't' Typeof
                   <|> parseCommandExpr 'e' Evaluate
                   <|> parseCommand 'q' QuitI
               )
        )
