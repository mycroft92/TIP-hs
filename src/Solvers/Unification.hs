module Solvers.Unification where

import AST.AST (Type (..), TypeVar)
import Solvers.UnionFindSolver

import qualified Data.Map as Map

import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    State,
    runState,
    -- StateT (runStateT),
 )

-- type Subst = Map.Map TypeVar Type

vars' :: Type -> [TypeVar]
vars' INT = []
vars' (Var i) = [i]
vars' (Points t) = vars' t
vars' (Arrow args ret) = concatMap vars' (ret : args)
vars' (Mu t typ) = filter (/= t) (vars' typ)

-- Need occurs check, apply substs to a term, union substs routines
occursCheck :: TypeVar -> Type -> Bool
occursCheck v (Var i) = i == v
occursCheck _ INT = False
occursCheck v (Points t) = occursCheck v t
occursCheck v (Arrow args ret) = checkargs
  where
    checkargs =
        foldl
            ( \acc arg -> case arg of
                Var v' -> (v == v') || acc
                _ -> acc
            )
            False
            (ret : args)
occursCheck v (Mu v' t) = (v /= v') && occursCheck v t

-- implements substition v/x in t
-- not checking capturing free variables
subst :: Type -> TypeVar -> Type -> Type
subst t@(Var x') x v
    | x' == x = v
    | otherwise = t
subst INT _ _ = INT
subst (Points t) x v = Points (subst t x v)
subst (Arrow args ret) x v = Arrow (map (\t -> subst t x v) args) (subst ret x v)
subst (Mu x' t) x v
    | x == x' = Mu x' t
    | otherwise = Mu x' (subst t x v)

unify' :: Substs Type -> Type -> Type -> Either String (Substs Type)
unify' ss t1 t2 = undefined

unify :: Type -> Type -> Either String (Substs Type)
unify = unify' Map.empty

type VarMap = Map.Map Int Int

data FreshState = FS
    { freshVar :: Int
    , varMap :: VarMap
    }

type Fresh a = State FreshState a

-- need a fresh monad setup for this
close :: Substs Type -> Type -> VarMap -> Type
close = undefined
