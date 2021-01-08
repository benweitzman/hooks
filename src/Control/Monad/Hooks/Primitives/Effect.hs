{-# LANGUAGE EmptyCase #-}

module Control.Monad.Hooks.Primitives.Effect where

import Control.Monad.Hooks.Class

data Effect a x where
  Effect :: Eq a => a -> IO (IO ()) -> Effect a ()

instance Hook (Effect a) where
   data instance HookState (Effect a) = EffectItem a (IO ())

   data instance AsyncUpdate (Effect a)

   updateState update = case update of {}

   step _ (Effect deps action) Nothing = do
     cleanup <- action
     return ((), EffectItem deps cleanup)
   step _ (Effect currentDeps action) (Just prevItem@(EffectItem prevDeps prevCleanup)) = do
     if currentDeps == prevDeps
     then return ((), prevItem)
     else do
       prevCleanup
       newCleanup <- action
       return ((), EffectItem currentDeps newCleanup)

   destroy (EffectItem _ cleanup) = cleanup
