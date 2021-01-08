module Lambda
  ( Expr (..),
    alphaEq,
    nf,
    betaEq,
    parseExpression,
    parseIdentifier,
    reduceOnce,
    Symb,
    unique,
    freeVars,
    subst,
  )
where

import Data.Set (Set, empty, fromList, insert, toList)
import Text.Parsec
  ( Parsec,
    alphaNum,
    char,
    lower,
    many,
    runParser,
    sepBy1,
    sepEndBy1,
    spaces,
    string,
    (<|>),
  )

type Symb = String
-- ^ Variables names

infixl 2 :@

data Expr
  = -- | Variable access
    Var Symb
  | -- | Expression sequence
    Expr :@ Expr
  | -- | Lambda
    Lam Symb Expr
  deriving (Eq)
-- ^ Lambda expression type

instance Show Expr where
  show (Var v) = v
  show (l :@ r) = "(" ++ show l ++ ") (" ++ show r ++ ")"
  show (Lam x m) = "\\" ++ x ++ " -> (" ++ show m ++ ")"

instance Read Expr where
  readsPrec _ input = case runParser parseExpression "" "" input of
    Left error -> []
    Right expr -> [(expr, "")]

unique :: Ord a => [a] -> [a]
-- ^ unique a - list of unique elements of a
unique xs = toList $ fromList xs

freeVars :: Expr -> [Symb]
-- ^ freeVars expr - free variables from expression expr
freeVars expr = unique $ freeVars' Data.Set.empty expr
  where
    freeVars' :: Set String -> Expr -> [Symb]
    freeVars' captured (Var name) = case (elem name captured) of
      True -> []
      False -> [name]
    freeVars' captured (l :@ r) = (freeVars' captured l) ++ (freeVars' captured r)
    freeVars' captured (Lam name ex) = freeVars' (Data.Set.insert name captured) ex

subst :: Symb -> Expr -> Expr -> Expr
-- ^ subst name new expr - substitute all free variable with name `name` occurences in expression expr to new
subst v n (Var name)
  | v == name = n
  | otherwise = Var name
subst v n (l :@ r) = (subst v n l) :@ (subst v n r)
subst v n l@(Lam varName body)
  | notElem v (freeVars l) = l
  | notElem varName $ freeVars n = Lam varName $ subst v n body
  | otherwise = Lam nextName $ subst v n nextBody
  where
    nextName = head $ filter (`notElem` (freeVars n ++ freeVars body)) $ map (varName ++) (map show [0 ..])
    nextBody = subst varName (Var nextName) body

alphaEq :: Expr -> Expr -> Bool
-- ^ a `alphaEq` b - if a is alpha equivalent to b
alphaEq (Var a) (Var b) = a == b
alphaEq (a :@ b) (x :@ y) = (alphaEq a x) && (alphaEq b y)
alphaEq (Lam fName fEx) (Lam sName sEx) = alphaEq (subst fName (Var $ '!' : fName) fEx) (subst sName (Var $ '!' : fName) sEx)
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
-- ^ reduceOnce a - apply one step of beta reduction. Just expr returned in case operation is appliable, Nothing otherwise
reduceOnce ((Lam x m) :@ n) = return $ subst x n m
reduceOnce (Var name) = Nothing
reduceOnce (l :@ r) = case reduceOnce l of
  Just redL -> Just $ redL :@ r
  Nothing -> do
    redR <- reduceOnce r
    return $ l :@ redR
reduceOnce (Lam x m) = do
  m <- reduceOnce m
  return $ Lam x m

nf :: Expr -> Expr
-- ^ nf expr Returns normal form of expr
nf expr = case reduceOnce expr of
  Just e -> nf e
  Nothing -> expr

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool
-- ^ a `betaEq` b - if a is beta equivalent to b
betaEq l r = nf l `alphaEq` nf r

parseIdentifier :: Parsec String String String
parseIdentifier = do
  firstLetter <- lower
  rest <- many alphaNum
  return $ firstLetter : rest

parseExpression :: Parsec String String Expr
parseExpression = do
  exprs <- sepBy1 (parseLambda <|> parseExpressionInBraces <|> parseVariable) spaces
  return $ foldl1 (:@) exprs

parseExpressionInBraces :: Parsec String String Expr
parseExpressionInBraces = do
  char '(' >> spaces
  e <- parseExpression
  spaces >> char ')'
  return e

parseVariable :: Parsec String String Expr
parseVariable = do
  name <- parseIdentifier
  return $ Var name

parseLambda :: Parsec String String Expr
parseLambda = do
  char '\\' >> spaces
  parameters <- sepEndBy1 parseIdentifier spaces
  spaces >> string "->" >> spaces
  expression <- parseExpression
  return $ foldr Lam expression parameters
