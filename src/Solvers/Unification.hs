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

-- close ::
