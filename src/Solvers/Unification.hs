module Solvers.Unification where

import AST.AST (Type (..), TypeVar)

vars' :: Type -> [TypeVar]
vars' INT = []
vars' (Var i) = [i]
vars' (Points t) = vars' t
vars' (Arrow args ret) = concatMap vars' (ret : args)
vars' (Mu t typ) = filter (/= t) (vars' typ)
