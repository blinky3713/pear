module Language.Types where

import Prelude hiding (lookup)
import Language.Names
import qualified Data.List as L

data Kind = Star | Kfun Kind Kind deriving (Eq, Show)

data Tyvar = Tyvar String Kind deriving (Eq, Show)

data Tycon = Tycon String Kind deriving (Eq, Show)

data Type =
    TVar Tyvar
  | TCon Tycon
  | TAp Type Type
  | TGen Int
  deriving (Eq, Show)

tUnit = TCon (Tycon "()" Star )
tChar = TCon (Tycon "Char" Star )
tInt = TCon (Tycon "Int" Star )
tNumber = TCon (Tycon "Number" Star )
tBool = TCon (Tycon "Bool" Star )
tArray = TCon (Tycon "[]" (Kfun Star Star ))
tString = TAp tArray tChar
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

-- helpers
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

infixr 4 `fn`

array :: Type -> Type
array = TAp tArray

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind t = case t of
    TVar a -> kind a
    TCon a -> kind a
    TAp a _ -> case kind a of
      Kfun _ a' -> a'
    TGen _ -> error "kind not implemented for TGen"

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar v) = case lookup v s of
    Just a -> a
    Nothing -> TVar v
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply s t = t

  tv (TVar tv) = [tv]
  tv (TAp l r) = tv l `L.union` tv r
  tv t = []

instance Types t => Types [t] where
  apply s = map (apply s)
  tv = L.nub . L.concatMap tv

--------------------------------------------------------------------------------
-- | Constraint system
--------------------------------------------------------------------------------

data Pred = IsIn Ident Type deriving (Eq, Show)

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t) = tv t

data Qual t = [Pred] :=> t deriving (Eq, Show)

newtype Subst = Subst [(Tyvar, Type)]

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `L.union` tv t

--------------------------------------------------------------------------------
-- | Substitutions
--------------------------------------------------------------------------------

lookup :: Tyvar -> Subst -> Maybe Type
lookup v (Subst s) = L.lookup v s

(|->) :: Tyvar -> Type -> Subst
v |-> t = Subst [(v,t)]

nullSubst :: Subst
nullSubst = Subst []

-- right action
instance Monoid Subst where
  mempty = nullSubst
  -- apply s1 then apply s2
  mappend (Subst s1) tao@(Subst s2) = Subst ([(u, apply tao t) | (u, t) <- s1] ++ s2)

--------------------------------------------------------------------------------
-- | Type Schemes
--------------------------------------------------------------------------------

data Scheme = Forall [Kind] (Qual Type) deriving (Eq, Show)

instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  tv (Forall _ t) = tv t
