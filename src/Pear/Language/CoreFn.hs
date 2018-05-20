module Pear.Language.CoreFn where

import Pear.Language.Utils
import qualified Pear.Language.Expr as E
import qualified Pear.Language.Names as N
import qualified Text.Parsec as P
import Pear.Language.Types
import qualified Pear.Language.Declaration as D

type Ann = (SourceSpan, Maybe Type)

ssAnn :: SourceSpan -> Ann
ssAnn ss = (ss, Nothing)

data Binder =
    NullBinder Ann
  | LiteralBinder Ann (E.Literal Binder)
  | VarBinder Ann N.Ident
  deriving (Show)

data Bind = NonRec Ann N.Ident Expr

data Expr
  = Literal Ann (E.Literal Expr)
  | Abs Ann N.Ident Expr
  | App Ann Expr Expr
  | Var Ann N.Ident
  | Case Ann [Expr] [CaseAlternative]
  deriving (Show)


data CaseAlternative = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: Either [(Expr, Expr)] Expr
  } deriving (Show)

-- | Desugars member declarations from AST to CoreFn representation.
declToCoreFn :: D.Declaration -> [Bind]
declToCoreFn (D.ValueDeclaration (D.ValueDeclarationData ss name _ e)) =
  [NonRec (ssAnn ss) name (exprToCoreFn ss Nothing e)]

exprToCoreFn :: SourceSpan -> Maybe Type -> E.Expr -> Expr
exprToCoreFn _ ty (E.Literal ss lit) =
  Literal (ss, ty) (fmap (exprToCoreFn ss Nothing) lit)
exprToCoreFn ss ty (E.Abs (N.VarBinder _ name) v) =
  Abs (ss, ty) name (exprToCoreFn ss Nothing v)
exprToCoreFn ss ty (E.App v1 v2) =
  App (ss, ty) (exprToCoreFn ss Nothing v1) (exprToCoreFn ss Nothing v2)
exprToCoreFn _ ty (E.Var ss ident) =
  Var (ss, ty) ident
exprToCoreFn ss ty (E.IfThenElse v1 v2 v3) =
  Case (ss, ty) [exprToCoreFn ss Nothing v1]
    [ CaseAlternative [LiteralBinder (ssAnn ss) $ E.BooleanLiteral True]
                      (Right $ exprToCoreFn ss Nothing v2)
    , CaseAlternative [NullBinder (ssAnn ss)]
                      (Right $ exprToCoreFn ss Nothing v3) ]
exprToCoreFn  _ _ e =
  error $ "Unexpected value in exprToCoreFn mn: " ++ show e

