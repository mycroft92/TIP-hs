module Analysis.NormDFS where

import AST.NAST

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

type Visitor s a = ExceptT String (StateT s IO) a
