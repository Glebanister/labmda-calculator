module Lambda where

import Data.Map (Map, empty, insert, lookup)
import Data.Set (Set, empty, insert)

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Read, Show)

type FreeVariableModifier = String -> Expr

type TiedVariableModifier = String -> String

modifyExpr :: FreeVariableModifier -> TiedVariableModifier -> Expr -> Expr
modifyExpr free tied ex = trav' free tied ex Data.Set.empty
  where
    trav' :: FreeVariableModifier -> TiedVariableModifier -> Expr -> (Set String) -> Expr
    trav' free tied (Var name) captured
      | elem name captured = Var (tied name)
      | otherwise = free name
    trav' free tied (left :@ right) captured = (trav' free tied left captured) :@ (trav' free tied right captured)
    trav' free tied (Lam name ex) captured = Lam (tied name) (trav' free tied ex (Data.Set.insert name captured))

modifyFree :: FreeVariableModifier -> Expr -> Expr
modifyFree func = modifyExpr func id

modifyTied :: TiedVariableModifier -> Expr -> Expr
modifyTied func = modifyExpr Var func

subst :: Symb -> Expr -> Expr -> Expr
subst name what to = modifyFree subst' (modifyTied (++ "'") to)
  where
    subst' varName | name == varName = modifyTied (++ "%") what
    subst' varName = Var varName

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var a) (Var b) = a == b
alphaEq (a :@ b) (x :@ y) = (alphaEq a x) && (alphaEq b y)
alphaEq (Lam fName fEx) (Lam sName sEx) = alphaEq (subst fName (Var $ "!" ++ fName ++ "!") fEx) (subst sName (Var $ "!" ++ fName ++ "!") sEx)
alphaEq _ _ = False
