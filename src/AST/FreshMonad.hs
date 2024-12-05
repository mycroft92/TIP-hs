module AST.FreshMonad where

--import Control.Monad.State.Lazy (State, runState)
import Control.Monad.State ( State, runState, MonadState (get, put))
import Control.Monad (when, void)
import Control.Applicative

type TypeVar = Int

type FreshMonad a = State TypeVar a

initType :: TypeVar
initType = 0

new :: FreshMonad TypeVar
new = do
    id <- get
    put (id+1)
    return id

