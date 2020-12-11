{-# LANGUAGE FlexibleContexts #-}

module Typing where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    StateT,
    evalStateT,
  )
import Data.List (intercalate, union)
import Data.Semigroup ()
import Data.Set (Set, empty, fromList, insert, toList)
import Lambda (Expr (..), Symb, freeVars, unique)

infixr 3 :->

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq)

instance Show Type where
  show t = fst $ showLen t
    where
      showLen :: Type -> (String, Integer)
      showLen (TVar t) = (t, 0)
      showLen (l :-> r) = case compare lenL 0 of
        EQ -> (strL ++ arrow ++ strR, lenR + 1)
        _ -> ("(" ++ strL ++ ")" ++ arrow ++ strR, lenR + 1)
        where
          arrow = " -> "
          (strL, lenL) = showLen l
          (strR, lenR) = showLen r

-- Контекст
newtype Env = Env [(Symb, Type)]
  deriving (Eq)

instance Show Env where
  show (Env ev) = intercalate "; " $ map (\(name, tp) -> name ++ " :: " ++ show tp) ev

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

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
      e2 <- equationsSt env n alpha
      return $ e1 ++ e2
    equationsSt env (Lam x m) tp = do
      alpha <- getNextType
      beta <- getNextType
      e <- equationsSt (extendEnv env x alpha) m beta
      return $ e ++ [(alpha :-> beta, tp)]

principlePair :: (MonadError String m) => Expr -> m (Env, Type)
principlePair expr = do
  let gamma0 = Env $ zip (freeVars expr) (map (\n -> TVar $ "b" ++ (show n)) [0 ..])
  let sigma0 = TVar "b"
  equs <- equations gamma0 expr sigma0
  let left = bindTypes $ map fst equs
  let right = bindTypes $ map snd equs
  subs <- unify left right
  return (appSubsEnv subs gamma0, appSubsTy subs sigma0)
  where
    bindTypes :: [Type] -> Type
    bindTypes ts = foldr1 (:->) ts
