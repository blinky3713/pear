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
  TypeConstructor _ -> S.empty

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

newtype TI a = TI {runTI :: ExceptT String (State Int) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Int)

execTI :: TI a -> Either String a
execTI action = runExceptT (runTI action) & flip evalState 0

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
mgu t (TypeVar u) = varBind u t
mgu (TypeVar u) t = varBind u t
mgu (TypeConstructor _) _ = pure nullSubs

varBind :: String -> Type -> TI Subs
varBind u t
  | t == TypeVar u = return nullSubs
  | u `S.member` freeTypeVars t = throwError $ "occurs check fails " ++ u ++ " vs. " ++ show t
  | otherwise = pure $ M.singleton u t

ti :: Environment -> Expr -> TI (Subs, Type)
ti (Environment e) (Var ss (Ident n)) =
  case M.lookup n e of
    Nothing -> throwError $ "Unbound variable: " ++ "Pos " ++ show ss ++ " , type name " ++ n
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubs, t)
ti _ (Literal _ lit) = pure (nullSubs, inferLit lit)
ti env (Abs (VarBinder _ (Ident n)) e2) = do
  tv <- newTyVar "a"
  let Environment env' = remove env n
      env'' = Environment (env' `M.union` (M.singleton n (Scheme [] tv)))
  (s1, t1) <- ti env'' e2
  pure $ (s1, TypeApp tyFunction (TypeApp (applySubs s1 tv) t1))
ti env exp@(App e1 e2) = do
    tv <- newTyVar "a"
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (applySubsEnv s1 env) e2
    s3 <- mgu (applySubs s2 t1) (TypeApp tyFunction (TypeApp t2 tv))
    pure $ (s3 `composeSubs` s2 `composeSubs` s1, applySubs s3 tv)
  `catchError` \e -> throwError $ e ++ "\n in " ++ show exp

typeInference :: M.Map String Scheme -> Expr -> TI Type
typeInference env e = do
  (s, t) <- ti (Environment env) e
  pure $ (applySubs s t)

testTypeChecker :: String -> IO ()
testTypeChecker str = do
  case parseExpr' str of
    Left e -> print $ "ParseError: " ++ show e
    Right expr -> case execTI (typeInference M.empty expr) of
      Left e -> print $ str ++ "\n " ++ e ++ "\n"
      Right t -> print $ str ++ " :: " ++ prettyPrintType t

--------------------------------------------------------------------------------
inferLit :: Literal Expr -> Type
inferLit lit = case lit of
  IntLiteral _ ->  tyInt
  NumericLiteral _ ->  tyNumber
  StringLiteral _ ->  tyString
  BooleanLiteral _ -> tyBoolean
  ArrayLiteral _ -> error "TODO: Array Literal"

foo :: Int -> Int
foo 1 = 2
foo x = x + x
