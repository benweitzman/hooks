{-# LANGUAGE EmptyCase #-}

module Control.Monad.Hooks.Primitives.Effect where

import Control.Monad.Hooks.Class

data Effect a m x where
  Effect :: Eq a => a -> m (m ()) -> Effect a m ()

instance Hook (Effect a) where
   data instance HookState (Effect a) m = EffectItem a (m ())

   data instance AsyncUpdate (Effect a) m

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
