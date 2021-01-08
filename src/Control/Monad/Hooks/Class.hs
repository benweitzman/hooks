module Control.Monad.Hooks.Class where

class Hook a where
  data family HookState a

  data family AsyncUpdate a

  updateState :: AsyncUpdate a -> HookState a -> HookState a

  step
    :: (AsyncUpdate a -> IO ())
    -> a x
    -> Maybe (HookState a)
    -> IO (x, HookState a)

  destroy :: HookState a -> IO ()