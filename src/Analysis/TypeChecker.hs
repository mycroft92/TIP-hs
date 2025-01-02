module Analysis.TypeChecker where

import Control.Exception (bracket)
import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
 )
import Data.Foldable (foldlM, foldrM)

import AST.AST

-- This is the only analysis we do on AST, rest all its on Normalized AST
data TypeState = TS
    {
    }

-- constraints
