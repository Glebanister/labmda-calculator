module Lambda where

import Data.Set (Set, empty, fromList, insert, toList)
import Text.Parsec

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Read, Show)

unique :: [String] -> [String]
unique xs = toList $ fromList xs

freeVars :: Expr -> [Symb]
freeVars expr = unique $ freeVars' Data.Set.empty expr
  where
    freeVars' :: Set String -> Expr -> [Symb]
    freeVars' captured (Var name) = case (elem name captured) of
      True -> []
      False -> [name]
    freeVars' captured (l :@ r) = (freeVars' captured l) ++ (freeVars' captured r)
    freeVars' captured (Lam name ex) = freeVars' (Data.Set.insert name captured) ex

subst :: Symb -> Expr -> Expr -> Expr
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
alphaEq (Var a) (Var b) = a == b
alphaEq (a :@ b) (x :@ y) = (alphaEq a x) && (alphaEq b y)
alphaEq (Lam fName fEx) (Lam sName sEx) = alphaEq (subst fName (Var $ '!' : fName) fEx) (subst sName (Var $ '!' : fName) sEx)
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
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
nf expr = case reduceOnce expr of
  Just e -> nf e
  Nothing -> expr

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool
betaEq l r = nf l `alphaEq` nf r

