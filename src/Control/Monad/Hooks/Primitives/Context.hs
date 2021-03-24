module Control.Monad.Hooks.Primitives.Context where

import Control.Monad.Hooks.Class
import GHC.TypeLits (Symbol)
import Control.Monad.Hooks.Runtime (Hooks(Use), HookExecutionHandle, HookExecutionHandle(HookExecutionHandle), terminate, runHooks)
import UnliftIO hiding (handle)

data Context (s :: Symbol) a m x where
  NamedContext :: forall s a m effects . Hooks m effects a -> Context s a m a

useNamedContext :: forall s a m effects . Hooks m effects a -> Hooks m '[Context s a] a
useNamedContext = Use . NamedContext

instance Hook (Context s a) where
  data instance HookState (Context s a) m = HiddenHandle
    { value :: a
    , handle :: HookExecutionHandle m
    }

  data instance AsyncUpdate (Context s a) m = SetHidden a

  updateState (SetHidden value) (HiddenHandle _ handle) = HiddenHandle
    { value
    , handle
    }

  destroy HiddenHandle { handle = HookExecutionHandle { terminate }} = terminate

  step dispatch (NamedContext hidden) Nothing = do
    mvar <- newEmptyMVar
    initialRef <- newIORef True
    handle <- runHooks hidden $ \result -> do
      isInitial <- readIORef initialRef
      if isInitial
      then putMVar mvar result >> writeIORef initialRef False
      else dispatch $ SetHidden result
    initialValue <- readMVar mvar
    return (initialValue, HiddenHandle initialValue handle)
  step _ (NamedContext _) (Just state@HiddenHandle { value }) = return (value, state)