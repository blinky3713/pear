module Language.Desugarer where

import Language.Expr
import Language.Names
import Language.Utils

-- The poin of the desugarer is to get rid of lambda abstraction and if then else

desugar :: MonadSupply m => Expr () -> m (Expr ())
desugar ex@(Abs b e) = do
  f <- Ident <$> freshName
  let bg = ([],[[(f, [([b], e)])]])
  pure $ Let bg (Var () f)
desugar ex@(IfThenElse i t e) = error "TODO: desugar"


