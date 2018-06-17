module Language.Types where


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
tFloat = TCon (Tycon "Number" Star )
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
