{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Kernel where

import Control.Monad (foldM)
import Control.Monad.Except (runExcept)
import Control.Monad.State (MonadState (get, state), MonadTrans (lift), State, StateT, modify)
import Control.Monad.State.Lazy (MonadState (put))
import Data.HashMap.Lazy (HashMap, empty, insert, lookup)
import Lambda (Expr, freeVars, nf, parseExpression, parseIdentifier, subst)
import System.IO (hFlush, stdout)
import Text.Parsec
  ( Parsec,
    char,
    runParser,
    spaces,
    string,
    (<|>),
  )
import Typing (Env, Type, principlePair)

type Scope = HashMap String Expr

empmtyScope :: Scope
empmtyScope = empty

getVar :: String -> Scope -> Maybe Expr
getVar name scope = Data.HashMap.Lazy.lookup name scope

addVar :: String -> Expr -> Scope -> Scope
addVar = insert

data Input
  = Assign String Expr
  | Evaluate Expr
  | Typeof Expr
  | QuitI

data Output
  = Result Expr
  | ExprType (Env, Type)
  | Error String String
  | Message String
  | QuitO
  | Ok

expandExpression :: Expr -> Scope -> Either String Expr
expandExpression expr scope =
  foldM
    ( \e varName -> case getVar varName scope of
        Nothing -> Left $ "name '" ++ varName ++ "' is not in scope"
        Just expr -> Right expr
    )
    expr
    (freeVars expr)

processLine :: String -> Scope -> StateT Scope IO Output
processLine line scope = do
  put scope
  case runParser parseInput "" "" line of
    Left error -> return $ Error "Parsing" $ show error
    Right (Assign name expr) -> case expandExpression expr scope of
      Left m -> return $ Error "UndefinedVariable" m
      Right expr -> do
        modify $ addVar name expr
        return Ok
    Right (Evaluate expr) -> case expandExpression expr scope of
      Left m -> return $ Error "UndefinedVariable" m
      Right expr -> return $ Result $ nf expr
    Right (Typeof expr) -> case expandExpression expr scope of
      Left m -> return $ Error "UndefinedVariable" m
      Right expr -> case runExcept $ principlePair expr of
        Left err -> return $ Error "Typing" err
        Right t -> return $ ExprType t
    Right QuitI -> return QuitO

instance Show Output where
  show (Result expr) = show expr
  show (ExprType (env, tp)) = "(" ++ show env ++ ")" ++ " => " ++ show tp
  show (Error name message) = name ++ "Error: " ++ message
  show (Message m) = m
  show QuitO = undefined
  show Ok = undefined

routine :: StateT Scope IO ()
-- ^ main routine iteration
routine = do
  lift $ putStr "λ> "
  lift $ hFlush stdout
  input <- lift getLine
  scope <- get
  res <- processLine input scope
  case res of
    QuitO -> return ()
    Ok -> routine
    out -> (lift $ putStrLn $ show out) >> routine

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
