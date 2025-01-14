module Solvers.UnionFindSolver where

--- try to follow the book here
--- Idea is to maintain equivalent terms concisely, instead of a map

import AST.AST (Type (..))
import qualified Data.Map as Map

type Substs a = Map.Map a a

mkSet :: (Ord a) => a -> Substs a -> Substs a
mkSet v ss = Map.insert v v ss

find :: (Ord a) => a -> Substs a -> a
find v ss =
    case Map.lookup v ss of
        Nothing -> v
        Just v' -> if v' == v then v else find v' ss

union :: (Ord a) => a -> a -> Substs a -> Substs a
union x y ss =
    let xr = find x ss
     in let yr = find y ss
         in if xr /= yr then Map.insert xr yr ss else ss

solution :: Substs Type -> Substs Type
solution ss = Map.filterWithKey checkKey ss
  where
    checkKey (Var _) _ = True
    checkKey _ _ = False

showSolution :: Substs Type -> String
showSolution ss = Map.foldl (\k v -> show k ++ " :: " ++ show v ++ "\n") "" (solution ss)
