module Language.TypeInference where

import Language.Entailment
import Language.Typechecker
import Language.Types
import Language.Expr
import qualified Data.List as L

--------------------------------------------------------------------------------
-- | Inference
--------------------------------------------------------------------------------

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiLit :: Infer (Literal (Expr a)) Type
tiLit _ _ (IntLiteral _) = pure ([], tInt)
tiLit _ _ (NumericLiteral _) = pure ([], tNumber)
tiLit _ _ (StringLiteral _) = pure ([], tString)
tiLit _ _ (BooleanLiteral _) = pure ([], tBool)
tiLit env ass (ArrayLiteral as) = do
    ts <- mapM (tiExpr env ass) as
    case ts of
      [] -> do
        v <- newTVar Star
        pure $ ([], TAp tArray v)
      (a : rest) -> do
        unifyExprs a rest
        s <- getSubst
        pure $ (concat $ apply s (map fst ts) , TAp tArray (apply s $ snd a))
  where
    unifyExprs x [] = return ()
    unifyExprs x (y:ys) = unify (snd x) (snd y) >> unifyExprs y ys


tiExpr :: Infer (Expr a) Type
tiExpr env as (Var _ i) = do
  sc <- find i as
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr env as (Literal _ lit) = tiLit env as lit
tiExpr env as (App l r) = do
  (ps, tl) <- tiExpr env as l
  (qs, tr) <- tiExpr env as r
  v <- newTVar Star
  unify (tr `fn` v) tl
  pure (ps `L.union` qs, v)
tiExpr env as (Abs b e) = undefined
