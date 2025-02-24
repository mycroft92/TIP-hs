module Solvers.Unification (close, unify, unify', substSoln, Solution, solution) where

import AST.AST (Type (..), TypeVar)
import Solvers.UnionFindSolver

import qualified Data.Map as Map

-- StateT (runStateT),

import Control.Conditional (ifM)

-- import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    -- MonadIO (liftIO),
    MonadState (get, put),
    -- MonadTrans (lift),
    State,
    runState,
 )
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

-- type Subst = Map.Map TypeVar Type
type Solution = Map.Map TypeVar Type

vars' :: Type -> [TypeVar]
vars' INT = []
vars' Abs = []
vars' (Var i) = [i]
vars' (Points t) = vars' t
vars' (Arrow args ret) = concatMap vars' (ret : args)
vars' (Mu t typ) = filter (/= t) (vars' typ)
vars' (Rec id_var_list) = concatMap (\(_, y) -> vars' y) id_var_list

-- Need occurs check, apply substs to a term, union substs routines
occursCheck :: TypeVar -> Type -> Bool
occursCheck v (Var i) = i == v
occursCheck _ INT = False
occursCheck _ Abs = False
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
solution :: Substs Type -> Solution
solution ss = recurseSubst (Map.foldrWithKey checkKey Map.empty ss)
  where
    checkKey (Var k) v acc = Map.insert k v acc
    checkKey _ _ acc = acc

    recurseSubst m = Map.foldrWithKey rec m m
    rec k (Var v) a = if Map.member v a then Map.insert k (fromJust (Map.lookup v a)) a else a
    rec k v a = a

showSolution :: Solution -> String
showSolution ss = Map.foldl (\k v -> "Var " ++ show k ++ " :: " ++ show v ++ "\n") "" ss

substSoln :: Type -> Solution -> Type
substSoln x ss = Map.foldrWithKey (\k v acc -> subst acc k v) x ss

subst :: Type -> TypeVar -> Type -> Type
subst t@(Var x') x v
    | x' == x = v
    | otherwise = t
subst INT _ _ = INT
subst Abs _ _ = Abs
subst (Points t) x v = Points (subst t x v)
subst (Arrow args ret) x v = Arrow (map (\t -> subst t x v) args) (subst ret x v)
subst (Rec fields) x v = Rec (map (\(f, t) -> (f, subst t x v)) fields)
subst (Mu x' t) x v
    | x == x' = Mu x' t
    | otherwise = Mu x' (subst t x v)

unify' :: Substs Type -> Type -> Type -> Either String (Substs Type)
unify' ss t1 t2 =
    let t1r = find t1 ss
     in let t2r = find t2 ss
         in if t1r /= t2r
                then
                    unify'' t1r t2r
                else Right ss
  where
    unify'' INT INT = return ss
    unify'' Abs Abs = return ss
    unify'' t1@(Var _) t2 = return $ union t1 t2 ss
    unify'' t1 t2@(Var _) = return $ union t2 t1 ss
    unify'' (Points t1) (Points t2) = unify' ss t1 t2
    -- unify'' case for Rec
    unify'' x@(Arrow args1 ret1) y@(Arrow args2 ret2)
        | length args1 /= length args2 = Left $ "unification failure, arity mismatch \n t1: " ++ show x ++ "\n t2: " ++ show y
        | otherwise = foldlM (\acc (t1, t2) -> unify' acc t1 t2) ss (zip (ret1 : args1) (ret2 : args2))
    -- Mu terms don't come at this stage, we need to close the unified terms to get Mu terms, hence this is ok
    unify'' t1 t2 = Left $ "unification failure t1: " ++ show t1 ++ " t2: " ++ show t2

unify :: Type -> Type -> Either String (Substs Type)
unify = unify' Map.empty

type VarMap = Map.Map TypeVar TypeVar

data FreshState = FS
    { fresh :: Int
    , freshvarMap :: VarMap
    , env :: Substs Type
    }

type Fresh a = State FreshState a

genFresh :: Fresh TypeVar
genFresh = do
    x <- get
    let fr = fresh x
    put (x{fresh = fr + 1})
    return fr

_checkvarcond :: TypeVar -> Set.Set TypeVar -> Fresh Bool
_checkvarcond x visited = do
    st <- get
    return (not (Set.member x visited) && find (Var x) (env st) /= Var x)

_getOrElse :: TypeVar -> Fresh Type
_getOrElse v = do
    st <- get
    let freshvars = freshvarMap st
    case Map.lookup v freshvars of
        Just x -> return (Var x)
        Nothing -> do
            fv <- genFresh
            put (st{freshvarMap = Map.insert v fv freshvars})
            return (Var fv)

_getFromFreshVars :: TypeVar -> Fresh (Maybe TypeVar)
_getFromFreshVars x = do
    st <- get
    return (Map.lookup x (freshvarMap st))

_getEnv :: Type -> Fresh Type
_getEnv t = do
    st <- get
    return (find t (env st))

closeRec :: Type -> Set.Set TypeVar -> Fresh Type
closeRec (Var v) visited = do
    ifM
        (_checkvarcond v visited)
        ( do
            -- guaranteed to not be Var v since it passes cond
            cterm' <- _getEnv (Var v)
            -- We close the inner term first
            cterm <- closeRec cterm' (Set.insert v visited)
            v' <- _getFromFreshVars v
            case v' of
                Nothing -> return cterm
                Just v'' -> if v'' `elem` vars' cterm then return (Mu v'' (subst cterm v (Var v''))) else return cterm
        )
        (_getOrElse v)
closeRec INT _ = return INT
closeRec (Points t) vst = do
    t' <- closeRec t vst
    return (Points t')
closeRec Abs _ = return Abs
closeRec (Mu v t) vst = do
    t' <- closeRec t vst
    return (Mu v t')
closeRec t@(Arrow args ret) vst =
    foldlM
        ( \acc v -> do
            closedv <- closeRec (Var v) vst
            return (subst acc v closedv)
        )
        t
        (vars' t)
closeRec t@(Rec fields) vst =
    foldlM
        ( \acc v -> do
            closedv <- closeRec (Var v) vst
            return (subst acc v closedv)
        )
        t
        (vars' t)

close :: Type -> Substs Type -> Type
close t env = case runState (closeRec t Set.empty) (FS m Map.empty env) of
    (t', _) -> t'
  where
    m = 1 + foldl max 0 (vars' t)
