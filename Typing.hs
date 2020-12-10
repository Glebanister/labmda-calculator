{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.List (nub, union)
import Data.Semigroup
import Data.Set (Set, empty, fromList, insert, toList)

type Symb = String

infixl 2 :@

infixr 3 :->

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Read, Show)

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

-- Контекст
newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

unique :: Ord a => [a] -> [a]
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

freeTVars :: Type -> [Symb]
freeTVars t = unique $ freeTVars' t
  where
    freeTVars' :: Type -> [Symb]
    freeTVars' (TVar t) = [t]
    freeTVars' (l :-> r) = (freeTVars' l) ++ (freeTVars' r)

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env en) sym tp = Env $ (sym, tp) : en

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env en) = unique $ do
  (n, t) <- en
  freeTVars t

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env []) v = throwError $ "There is no variable \"" ++ v ++ "\" in the enviroment."
appEnv (Env ((name, t) : xs)) v
  | v == name = return t
  | otherwise = appEnv (Env xs) v

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy ((sym, tp) : xs)) (TVar name)
  | sym == name = tp
  | otherwise = appSubsTy (SubsTy xs) (TVar name)
appSubsTy subst (l :-> r) = (appSubsTy subst l) :-> (appSubsTy subst r)

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subs (Env en) = Env $ do
  (name, t) <- en
  return (name, appSubsTy subs t)

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy t) (SubsTy s) = SubsTy $ do
  (name, tp) <- t `union` s
  return (name, appSubsTy (SubsTy t) (appSubsTy (SubsTy s) (TVar name)))

instance Semigroup SubsTy where
  a <> b = a `composeSubsTy` b

instance Monoid SubsTy where
  mempty = SubsTy []
  mappend = (<>)

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar t) (TVar p) | t == p = return mempty
unify (TVar t) tp
  | elem t (freeTVars tp) = throwError $ "Can't unify (" ++ (show (TVar t)) ++ ") with (" ++ (show tp) ++ ")!"
  | otherwise = return $ SubsTy [(t, tp)]
unify (l :-> r) (TVar t) = unify (TVar t) (l :-> r)
unify (s1 :-> s2) (t1 :-> t2) = do
  u2 <- unify s2 t2
  u1 <- (unify (appSubsTy u2 s1) (appSubsTy u2 t1))
  return $ u1 <> u2

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type, Type)]
equations env expr tp = evalStateT (equationsSt env expr tp) 0
  where
    getNextType :: MonadState Integer m => m Type
    getNextType = do
      index <- get
      put (succ index)
      return $ TVar ('a' : show index)
    equationsSt :: MonadError String m => Env -> Expr -> Type -> StateT Integer m [(Type, Type)]
    equationsSt env (Var name) tp = do
      varType <- appEnv env name
      return [(tp, varType)]
    equationsSt env (m :@ n) tp = do
      alpha <- getNextType
      e1 <- equationsSt env m (alpha :-> tp)
      e2 <- equationsSt env m alpha
      return $ e1 ++ e2
    equationsSt env (Lam x m) tp = do
      alpha <- getNextType
      beta <- getNextType
      e <- equationsSt (extendEnv env x tp) m beta
      return $ e ++ [(alpha :-> beta, tp)]
