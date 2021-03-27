{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Hooks.Primitives.Context where

import Control.Monad.Hooks.Class
import GHC.TypeLits (Symbol)
import Control.Monad.Hooks.Runtime (Hooks(Use), HookExecutionHandle, HookExecutionHandle(HookExecutionHandle), terminate, runHooks)
import UnliftIO hiding (handle)
import Data.Kind (Type)

data Context (c :: Type) m x where
  Context :: forall c m effects . Deps c -> Hooks m effects (Outputs c) -> Context c m (Outputs c)

type family Deps (c :: Type) :: Type
type family Outputs (c :: Type) :: Type

data AnonymousContext (s :: Symbol) (deps :: Type) (out :: Type)

type instance Deps (AnonymousContext s deps out) = deps
type instance Outputs (AnonymousContext _ _ out) = out

type (:~>) = Context

useContext :: forall c m effects . Eq (Deps c) => (Deps c -> Hooks m effects (Outputs c)) -> Deps c -> Hooks m '[Context c] (Outputs c)
useContext hook deps = Use (Context deps (hook deps))

instance Eq (Deps c) => Hook (Context c) where
  data instance HookState (Context c) m = HiddenHandle
    { value :: Outputs c
    , dependencies :: Deps c
    , handle :: HookExecutionHandle m
    }

  data instance AsyncUpdate (Context c) m = SetHidden (Outputs c)

  updateState (SetHidden value) (HiddenHandle _ dependencies handle) = HiddenHandle
    { value
    , dependencies
    , handle
    }

  destroy HiddenHandle { handle = HookExecutionHandle { terminate }} = terminate

  step dispatch (Context deps hidden) mPrevState = case mPrevState of
    Nothing -> createSubContext
    Just state@(HiddenHandle value prevDeps handle)
      | prevDeps == deps -> return (value, state)
      | otherwise -> do
          terminate handle
          createSubContext

    where

      createSubContext = do
         mvar <- newEmptyMVar
         initialRef <- newIORef True
         handle <- runHooks hidden $ \result -> do
           isInitial <- readIORef initialRef
           if isInitial
           then putMVar mvar result >> writeIORef initialRef False
           else dispatch $ SetHidden result
         initialValue <- readMVar mvar
         return (initialValue, HiddenHandle initialValue deps handle)