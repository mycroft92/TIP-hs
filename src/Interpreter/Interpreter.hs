module Interpreter.Interpreter where

import AST.NAST
import Interpreter.SemanticValues

import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.State (
    MonadIO (liftIO),
    MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
 )
import Data.Foldable (foldrM)
import Data.IORef
import Interpreter.Environment (Env (..))

data InterpreterState = InterpreterState
    { env :: IORef Env
    , globals :: Env -- why is this problematic with IOREFs?
    --    , functionEnvs :: Map.Map Value (Env, Decl)
    -- , locals :: Map.Map Expr Int
    }

type Interpreter a = ExceptT InterpreterException (StateT InterpreterState IO) a
