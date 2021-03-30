{-# LANGUAGE EmptyCase #-}

module Control.Monad.Hooks.Primitives.Ref where

import Control.Monad.Hooks.Class
import Control.Monad.Hooks.Runtime (Hooks(Use))
import Control.Monad.Hooks.Stateful
import UnliftIO

data Ref (a :: *) (m :: * -> *) x where
  Ref :: forall a m . a -> Ref a m (Escaping (IORef a))

useRef :: forall a m . a -> Hooks m '[Ref a] (Escaping (IORef a))
useRef initial = Use $ Ref initial

instance Hook (Ref a) where
  data instance HookState (Ref a) m = RefState (IORef a)

  data instance AsyncUpdate (Ref a) m

  updateState s = case s of {}

  destroy _ = return ()

  step _ (Ref initial) Nothing = do
    ref <- newIORef initial
    return (Escaping ref, RefState ref)
  step _ (Ref _) (Just (RefState ref)) = do
    return (Escaping ref, RefState ref)