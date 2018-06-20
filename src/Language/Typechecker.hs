{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Typechecker where

import Prelude hiding (lookup)
import Language.Types
import qualified Data.List as L
import Data.Monoid
import Control.Monad.State
import Language.Utils
import Language.Expr
import Language.Names

-- Unification

merge :: Monad m => Subst -> Subst -> m Subst
merge sig@(Subst s1) tao@(Subst s2) =
    if agree
      then return $ Subst (s1 ++ s2)
      else fail "merge fails"
  where
    agree = all (\v -> apply sig (TVar v) == apply tao (TVar v))
      (map fst s1 `L.intersect` map fst s2)

-- unify a type with a type variable, preserving kinds and
-- avoiding constructing the infinite type (occurs check)
varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `L.elem` tv t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return (u |-> t)

-- mgu t1 t2 is a substitusion s such that s t1 == s t2 where
-- s is prime, and all other unifiers u (with u t1 == u t2) contain
-- s a a factor.
mgu :: Monad m => Type -> Type -> m Subst
mgu (TAp l1 r1) (TAp l2 r2) = do
  s1 <- mgu l1 l2
  s2 <- mgu (apply s1 r1) (apply s1 r2)
  return (s1 <> s2)
mgu (TVar v) t = varBind v t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu _ _ = fail "types do not unify"


mguPred :: Pred -> Pred -> Maybe Subst
mguPred (IsIn i1 t1) (IsIn i2 t2) =
  if i1 == i2 then mgu t1 t2 else Nothing

-- match t1 t2 is a subs s such that s t1 == t2
match :: Monad m => Type -> Type -> m Subst
match (TAp l1 r1) (TAp l2 r2) = do
  sl <- match l1 l2
  sr <- match r1 r2
  merge sl sr
match (TVar v) t | kind v == kind t = return $ v |-> t
match (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
match _ _  = fail "types do not match"

matchPred :: Pred -> Pred -> Maybe Subst
matchPred (IsIn i1 t1) (IsIn i2 t2) =
  if i1 == i2 then match t1 t2 else Nothing

--------------------------------------------------------------------------------
-- Type Inference Monad
--------------------------------------------------------------------------------

newtype TI a = TI { runTI :: State (Subst, Int) a }
  deriving (Functor, Applicative, Monad, MonadState (Subst, Int))

instance MonadSupply TI where
  fresh = do
    (s, n) <- get
    put (s, n + 1)
    return n

evalTI :: TI a -> a
evalTI = flip evalState (nullSubst, 0) . runTI

getSubst :: TI Subst
getSubst = fst <$> get

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  s' <- mgu (apply s t1) (apply s t2)
  extendSubst s'

extendSubst :: Subst -> TI ()
extendSubst s = do
  (s', n) <- get
  put (s' <> s, n)

newTVar :: Kind -> TI Type
newTVar k = do
  (s,n) <- get
  put (s, n + 1)
  pure . TVar $ Tyvar ("v" ++ show n) k

--------------------------------------------------------------------------------

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [ v | v <- tv qt, v `L.elem` vs]
    ks = map kind vs'
    s = Subst $ zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return $ inst ts qt

class Instantiate t where
  inst :: [Type] -> t -> t


instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen i) = ts !! i
  inst _ t = t

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> h) = inst ts ps :=> inst ts h

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

instance Instantiate t => Instantiate [t] where
  inst ts = map (inst ts)

--------------------------------------------------------------------------------

data Assump = Ident :>: Scheme

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc) = tv sc

find :: Monad m => Ident -> [Assump] -> m Scheme
find (Ident i) [] = fail ("unbound identifier: " ++ i)
find i ((i':>: sc) : as) = if i == i then return sc else find i as
