module Language.Entailment where

import Data.Maybe
import Control.Monad
import Language.Types
import Language.Names
import Language.Typechecker

--------------------------------------------------------------------------------
-- | Classes
--------------------------------------------------------------------------------

-- | ([superclasses], [instances])
type Class = ([Ident], [Inst])

-- | assumptions => instance head
type Inst = Qual Pred

data ClassEnv =
  ClassEnv { classes :: Ident -> Maybe Class
           , defaults :: [Type]
           }

-- get all superclasses of a class by name
super :: ClassEnv -> Ident -> [Ident]
super env i = case classes env i of
  Nothing -> []
  Just (ids, insts) -> ids

-- | get all instances for a class
insts :: ClassEnv -> Ident -> [Inst]
insts env i = case classes env i of
  Nothing -> []
  Just (ids, insts) -> insts

modifyClassEnv :: ClassEnv -> Ident -> Class -> ClassEnv
modifyClassEnv env ident c =
  let classes' = \i -> if i == ident then Just c else classes env i
  in env {classes = classes'}

emptyClassEnv :: ClassEnv
emptyClassEnv =
  ClassEnv { classes = \i -> Nothing
           , defaults = []
           }

defined :: Ident -> ClassEnv -> Bool
defined i env = isJust $ classes env i

type EnvTransformer = ClassEnv -> Maybe ClassEnv

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

infixr 5 <:>

-- Add a class with the name and list of superclasses,
-- fails if name already taken or a superclass doesn't exist
-- TODO: better error message if superclass doesn't exist
addClass :: Ident -> [Ident] -> EnvTransformer
addClass i is env
  | defined i env = fail $ "Class already defined: " ++ show i
  | any (not . isJust . classes env) is = fail "Superclass not defined"
  | otherwise = return $ modifyClassEnv env i (is,[])

addCoreClasses :: EnvTransformer
addCoreClasses =
      addClass (Ident "Eq") [ ]
  <:> addClass (Ident "Ord") [Ident "Eq"]
  <:> addClass (Ident "Show") []
  <:> addClass (Ident "Read") []
  <:> addClass (Ident "Bounded") []
  <:> addClass (Ident "Enum") []
  <:> addClass (Ident "Functor") []
  <:> addClass (Ident "Monad") [ ]


-- instance ps => p where ...
-- NOTE: every instance keeps track of all all superclass instances?
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) env
    | not (defined i env) = fail "no class for instance"
    | any (overlap p) instanceHeads = fail "overlapping instance"
    | otherwise = return $ modifyClassEnv env i c
  where
    overlap :: Pred -> Pred -> Bool
    overlap p q = isJust (mguPred p q)
    c = (super env i, (ps :=> p) : instances)
    instanceHeads  = [q | (_ :=> q) <- instances]
    instances = insts env i

--------------------------------------------------------------------------------
-- | Entailment
--------------------------------------------------------------------------------

-- the following two functions help us collect all the things we need to prove

-- bySuper means takes an environment and a predicate p, and gives a list of
-- all predicates that must hold via superclass relations.
-- e.g. super env (IsIn (Ident "Ord" a)) = (IsIn (Ident "Ord" a)) : (IsIn (Ident "Eq" a)) : []
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper env p@(IsIn i t) =
  p : concat [bySuper env (IsIn i' t) | i' <- super env i]

-- take an environment and a predicate,
-- get all the instances for the class, and try to unify the head with the type
-- an existing instance, and return all the predicates for that instance. Because
-- no overlapping instances are allowed, we know this is there is only one match.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst env p@(IsIn i t) = msum [tryInst it | it <- insts env i]
  where
    tryInst :: Qual Pred -> Maybe [Pred]
    tryInst (ps :=> h) = do
      u <- matchPred h p
      Just $ map (apply u) ps

-- if p is entailed using only superclasses of the assumptions, we're good.
-- otherwise, we get all the propositions we would need to prove if we're going
-- to entail via instance declaration, and show that the assumptions prove all
-- of these things.
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail env ps p =
  let byS = any (p `elem`) (map (bySuper env) ps)
      byI = case byInst env p of
        Nothing -> False
        Just qs -> all (entail env ps) qs
  in byS || byI

--------------------------------------------------------------------------------
-- | Context Reduction
--------------------------------------------------------------------------------

-- NOTE: This is an optimization, but an interesting one

-- A Predicate is in Head Normal Form if it is a type variable, or a type
-- variable applied to one or more types.
-- NOTE: Why?
inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where
    hnf (TVar v) = True
    hnf (TCon tc) = False
    hnf (TAp t _) = hnf t

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs env ps = concat <$> mapM (toHnf env) ps

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf env p
  | inHnf p = return [p]
  | otherwise = case byInst env p of
      Nothing -> fail "context reduction"
      Just ps -> toHnfs env ps

-- eliminate duplication (greedy)
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop rs [] = rs
    loop rs (p : ps)
      | entail ce (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps


reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do
  qs <- toHnfs ce ps
  return $ simplify ce qs
