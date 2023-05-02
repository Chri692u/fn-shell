{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ScriptInfer where

import Prelude hiding (foldr)
import Data.Monoid
import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

import ScriptSyntax
import ScriptTypes

-- Type variable environment
newtype TypeEnv = TypeEnv (M.Map Id Scheme) deriving (Monoid, Semigroup)

-- Unique type variables
newtype Unique = Unique { count :: Int }

-- Error handling
type Infer = ExceptT TypeError (State Unique)

-- Substitution is a mapping from type variables to types
type Subst = M.Map TVar Type

-- Apply substitution and query free variables
class Substitutable a where
    apply :: Subst -> a -> a
    free   :: a -> S.Set TVar

-- Substitution rules for Type 
instance Substitutable Type where
    apply _ (TCon a) = TCon a
    apply s t@(TVar a) = M.findWithDefault t a s
    apply s (t1 `TArr` t2) = t1' `TArr` t2'
        where t1' = apply s t1
              t2' = apply s t2

    free TCon{} = S.empty
    free (TVar a) = S.singleton a
    free (t1 `TArr` t2) = S.union t1' t2'
        where t1' = free t1
              t2' = free t2

-- Substitution rules for type scheme
instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
        where s' = foldr M.delete s as
    free (Forall as t) = S.difference t' as'
        where t' = free t
              as' = S.fromList as

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    free = foldr (S.union . free) S.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
    free (TypeEnv env) = free $ M.elems env

data TypeError = UnificationFail Type Type
               | InfiniteType TVar Type
               | UnboundVariable String

instance Show TypeError where
    show (UnificationFail t1 t2) = "Script typing error: Failed to unify type (" ++ show t1 ++ ") with type (" ++ show t2 ++ ")."
    show (InfiniteType tvar t) = "Script typing error: Type variable (" ++ show tvar ++ ") is already bound to (" ++ show t ++ ")."
    show (UnboundVariable name) = "Script typing error: Attempted to lookup type variable that is unbound: " ++ name ++ ")."

-- Initial unique type variables
initUnique :: Unique
initUnique = Unique { count = 0 }

-- A list of letters for type variables
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- Operator type mappings
ops :: Binop -> Type
ops Add = typeInt `TArr` typeInt `TArr` typeInt
ops Mul = typeInt `TArr` typeInt `TArr` typeInt
ops Sub = typeInt `TArr` typeInt `TArr` typeInt
ops Eql = typeInt `TArr` typeInt `TArr` typeBool

-- Empty TypeEnv
emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv M.empty

-- Get a fresh type variable
fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

-- Extend the TypeEnv with a type scheme
extend :: TypeEnv -> (Id, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

-- Lookup a variable in the type environment
lookupEnv :: TypeEnv -> Id -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (M.empty, t)

-- Type jugdement on a type variable in a TypeEnv
judgement :: TypeEnv -> Id -> Maybe Scheme
judgement (TypeEnv env) name = M.lookup name env

-- Compose substitution (Left biased substitution)
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

-- Is a type variable occurs free in TVar
occurs :: Substitutable p => TVar -> p -> Bool
occurs a t = S.member a t'
    where t' = free t

-- Unify types
unify :: Type -> Type -> Infer Subst
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return M.empty
unify (l1 `TArr` r1) (l2 `TArr` r2)  = do
    s1 <- unify l1 l2
    s2 <- unify (apply s1 r1) (apply s1 r2)
    return $ compose s1 s2
unify t1 t2 = throwError $ UnificationFail t1 t2

-- Bind type to type variable
bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a = return M.empty
         | occurs a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

-- get normal form of types
normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
    where
        ord = zip (nub $ fv body) (fmap TV letters)

        fv (TVar a) = [a]
        fv (TArr a b) = fv a ++ fv b
        fv (TCon _)   = []

        normtype (TArr a b) = TArr (normtype a) (normtype b)
        normtype (TCon a)   = TCon a
        normtype (TVar a)   = case lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature" 

-- Instantiation: Convert a type scheme to a type
-- Create fresh names for each type variable
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = M.fromList $ zip as as'
    return $ apply s t

-- Generalization: Convert a type into a type scheme
-- Close over all free type variables
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = S.toList $ free t `S.difference` free env

closeOver :: (M.Map TVar Type, Type) -> Scheme
closeOver (s, t) = normalize sc 
    where sc = generalize emptyTyenv (apply s t)

-- Infer primitive type
inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env exprs t = do
  var <- fresh
  (s1, tf) <- foldM inferStep (M.empty, id) exprs
  s2 <- unify (apply s1 (tf var)) t
  return (compose s2 s1, apply s2 var)
  where
  inferStep (s, tf) expr = do
    (s', t) <- infer (apply s env) expr
    return (compose s' s, tf . TArr t)

-- Inference algorithm
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env expr = case expr of
    -- Infer variable type
    Var var -> lookupEnv env var

    -- Infer operator type
    Op op e1 e2 -> inferPrim env [e1, e2] (ops op)

    -- Infer fixpoint
    Fix e -> do
        var <- fresh
        inferPrim env [e] ((var `TArr` var) `TArr` var)

    -- Infer if then else
    If cond tr fl -> do
        var <- fresh
        inferPrim env exprs (typeBool `TArr` var `TArr` var `TArr` var)
            where exprs = [cond, tr, fl]

    -- Infer let binding
    Let var e1 e2 -> do
        (s1, t1) <- infer env e1
        let env' = apply s1 env
            t' = generalize env' t1
        (s2, t2) <- infer (extend env (var, t')) e2
        return (compose s2 s1, t2)

    -- Infer lambda abstraction
    Lam var expr -> do
        tvar <- fresh
        let env' = extend env (var, Forall [] tvar)
        (s, t) <- infer env' expr
        return (s, apply s tvar `TArr` t)

    -- Infer application
    App e1 e2 -> do
        tvar <- fresh
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (apply s1 env) e2
        s3 <- unify (apply s2 t1) (TArr t2 tvar)
        return (compose s3 $ compose s2 s1, apply s3 tvar)

    -- Infer literals
    Lit (LInt _)  -> return (M.empty, typeInt)
    Lit (LBool _) -> return (M.empty, typeBool)

-- Run inference algorithm
runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

-- Top level
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, expr):xs) = case inferExpr env expr of
  Left err -> Left err
  Right t -> inferTop (extend env (name, t)) xs
