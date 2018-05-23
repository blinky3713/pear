module Pear.Language.TypeChecker where

import Pear.Language.Names
import Pear.Language.Types
import Pear.Language.Expr
import Pear.Language.Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function ((&))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except



-- polytype (eg. forall a. a -> int)
data Scheme = Scheme [String] Type deriving (Eq, Show)

freeTypeVars :: Type -> S.Set String
freeTypeVars ty = case ty of
  TypeVar n -> S.singleton n
  TypeApp t1 t2 -> freeTypeVars t1 `S.union` freeTypeVars t2

freeTypeVarsS :: Scheme -> S.Set String
freeTypeVarsS (Scheme vars t) = freeTypeVars t S.\\ S.fromList vars

type Subs = M.Map String Type

nullSubs :: Subs
nullSubs = M.empty

composeSubs :: Subs -> Subs -> Subs
composeSubs s1 s2 = (M.map (applySubs s1) s2) `M.union` s1

applySubs :: Subs -> Type -> Type
applySubs subs (TypeVar n) = case M.lookup n subs of
  Nothing -> TypeVar n
  Just t -> t
applySubs subs (TypeApp t1 t2) = TypeApp (applySubs subs t1) (applySubs subs t2)
applySubs subs tc@(TypeConstructor _) = tc

applySubsS :: Subs -> Scheme -> Scheme
applySubsS subs (Scheme vars t) = Scheme vars $ applySubs (foldr M.delete subs vars) t

newtype Environment = Environment (M.Map String Scheme) deriving (Eq, Show)

remove :: Environment -> String -> Environment
remove (Environment e) var = Environment $ M.delete var e

freeTypeVarsEnv :: Environment -> S.Set String
freeTypeVarsEnv (Environment e) = foldr S.union S.empty $ map freeTypeVarsS $ M.elems e

applySubsEnv :: Subs -> Environment -> Environment
applySubsEnv subs (Environment e) = Environment $ M.map (applySubsS subs) e

-- abstracts a type over all variables which are free in that type but not free in the
-- environment
generalize :: Environment -> Type -> Scheme
generalize env t =
  let vars = S.toList (freeTypeVarsEnv env S.\\ freeTypeVars t)
  in Scheme vars t

newtype TI a = TI {runTI :: ExceptT String (ReaderT Environment (State Int)) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader Environment, MonadState Int)

execTI :: Environment -> TI a -> Either String a
execTI e action = runExceptT (runTI action) &flip runReaderT e & flip evalState 0

newTyVar :: String -> TI Type
newTyVar prefix = do
  i <- get
  modify ((+) 1)
  pure . TypeVar $ prefix ++ show i

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  tVars <- mapM newTyVar vars
  let subs = M.fromList (zip vars tVars)
  return $ applySubs subs t

mgu :: Type -> Type -> TI Subs
mgu (TypeApp l r) (TypeApp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (applySubs s1 r) (applySubs s1 r')
  return $ s1 `composeSubs` s2
mgu (TypeVar u) t = varBind u t
  where
    varBind :: String -> Type -> TI Subs
    varBind u t
      | t == TypeVar u = return nullSubs
      | u `S.member` freeTypeVars t = throwError $ "occurs check fails " ++ u ++ " vs. " ++ show t
      | otherwise = pure $ M.singleton u t
mgu (TypeConstructor _) _ = pure nullSubs
--------------------------------------------------------------------------------

infer :: Expr -> Expr
infer expr = case expr of
  Literal ss lit -> inferLit ss lit

inferLit :: SourceSpan -> Literal Expr-> Expr
inferLit ss lit = case lit of
  IntLiteral v -> TypedValue (Literal ss $ IntLiteral v) tyInt
  NumericLiteral v -> TypedValue (Literal ss $ NumericLiteral v) tyNumber
  StringLiteral v -> TypedValue (Literal ss $ StringLiteral v) tyString
  BooleanLiteral v -> TypedValue (Literal ss $ BooleanLiteral v) tyBoolean
  ArrayLiteral v -> error "TODO: Array Literal"
