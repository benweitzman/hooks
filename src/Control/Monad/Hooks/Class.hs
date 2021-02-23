{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Hooks.Class where

import Data.Kind
import UnliftIO

class Hook a where
  data family HookState a :: (Type -> Type) -> Type

  data family AsyncUpdate a :: (Type -> Type) -> Type

  updateState :: AsyncUpdate a m -> HookState a m -> HookState a m

  step
    :: (MonadUnliftIO m)
    => (AsyncUpdate a m -> m ())
    -> a m x
    -> Maybe (HookState a m)
    -> m (x, HookState a m)

  destroy :: Monad m => HookState a m -> m ()