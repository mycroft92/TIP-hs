module Solvers.Unification where

import AST.AST (Type (..), TypeVar)
import qualified Data.Map as Map

type Subst = Map.Map TypeVar Type

vars' :: Type -> [TypeVar]
vars' INT = []
vars' (Var i) = [i]
vars' (Points t) = vars' t
vars' (Arrow args ret) = concatMap vars' (ret : args)
vars' (Mu t typ) = filter (/= t) (vars' typ)

-- Need occurs check, apply substs to a term, union substs routines
occursCheck :: TypeVar -> Type -> Bool
occursCheck = undefined

-- unify' ::
