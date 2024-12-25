module AST.Normalization where

import AST.NAST
import AST.AST
import Data.Foldable (foldlM, foldrM)
import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get,put),
      MonadTrans(lift))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
import Control.Monad (forM)

data NormState = NormState {
    freshvar :: Int,
    id_list  :: [String],
    stmts    :: [NStmt]
                           } deriving (Eq, Show)

type Normalize a = ExceptT String (StateT NormState IO) a
