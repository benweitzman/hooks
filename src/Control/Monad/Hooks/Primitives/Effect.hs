{-# LANGUAGE EmptyCase #-}

module Control.Monad.Hooks.Primitives.Effect where

import Control.Monad.Hooks.Stateful
import Control.Monad.Hooks.Class
import Control.Monad.Hooks.Runtime (Hooks(Use))

data Effect a m x where
  Effect :: Eq a => Stateful a -> (a -> m (m ())) -> Effect a m ()

useEffect :: Eq a => Stateful a -> (a -> m (m ())) -> Hooks m '[Effect a] ()
useEffect key eff = Use $ Effect key eff

once :: m (m ()) -> Hooks m '[Effect ()] ()
once = useEffect (Stateful ()) . const

instance Hook (Effect a) where
   data instance HookState (Effect a) m = EffectItem a (m ())

   data instance AsyncUpdate (Effect a) m

   updateState update = case update of {}

   step _ (Effect (Stateful deps) action) Nothing = do
     cleanup <- action deps
     return ((), EffectItem deps cleanup)
   step _ (Effect (Stateful currentDeps) action) (Just prevItem@(EffectItem prevDeps prevCleanup)) = do
     if currentDeps == prevDeps
     then return ((), prevItem)
     else do
       prevCleanup
       newCleanup <- action currentDeps
       return ((), EffectItem currentDeps newCleanup)

   destroy (EffectItem _ cleanup) = cleanup
