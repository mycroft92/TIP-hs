module Solvers.Unification where

import AST.AST (Type (..), TypeVar)
import Solvers.UnionFindSolver

-- import qualified Data.Map as Map

-- type Subst = Map.Map TypeVar Type

vars' :: Type -> [TypeVar]
vars' INT = []
vars' (Var i) = [i]
vars' (Points t) = vars' t
vars' (Arrow args) = concatMap vars' args
vars' (Mu t typ) = filter (/= t) (vars' typ)

-- Need occurs check, apply substs to a term, union substs routines
occursCheck :: TypeVar -> Type -> Bool
occursCheck v (Var i) = i == v
occursCheck _ INT = False
occursCheck v (Points t) = occursCheck v t
occursCheck v (Arrow args) = checkargs
  where
    checkargs =
        foldl
            ( \acc arg -> case arg of
                Var v' -> (v == v') || acc
                _ -> acc
            )
            False
            args
occursCheck v (Mu v' t) = (v /= v') && occursCheck v t

-- implements substition v/x in t
-- not checking capturing free variables
subst :: Type -> TypeVar -> Type -> Type
subst t@(Var x') x v
    | x' == x = v
    | otherwise = t
subst INT _ _ = INT
subst (Points t) x v = Points (subst t x v)
subst (Arrow args) x v = Arrow (map (\t -> subst t x v) args)
subst (Mu x' t) x v
    | x == x' = Mu x' t
    | otherwise = Mu x' (subst t x v)

-- findSubst :: TypeVar -> Subst -> Type
-- findSubst v ss =
--     case Map.lookup v ss of
--         Nothing -> Var v
--         Just (Var v') -> findSubst v' ss
--         Just t -> t
--
-- -- flying blind here, I don't know the correct approach
-- applySubst :: Subst -> Type -> Type
-- applySubst ss (Var n) = findSubst n ss
-- applySubst _ INT = INT
-- applySubst ss (Points t) = Points (applySubst ss t)
-- applySubst ss (Arrow args) = Arrow (map (applySubst ss) args)
-- applySubst ss (Mu v' t) = Mu v' (applySubst ss' t)
--   where
--     ss' = Map.delete v' ss

-- unify' :: Type -> Type -> Subst -> Either (String, String) Subst
-- unify' t1@(Var v1) t2@(Var v2) ss =
--     let v1r = findSubst v1 ss in
--     let v2r = findSubst v2 ss in
--     case v2r of
--         Var _ -> Right v1r
--         _ ->  Right v2r
