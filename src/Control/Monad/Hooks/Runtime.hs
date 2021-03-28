{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Hooks.Runtime where

import Control.Monad.Hooks.Class
import Control.Monad.Hooks.List
import Control.Monad.Hooks.Stateful

import Data.Proxy (Proxy(..))
import UnliftIO

data Hooks m effects a where
  Use :: (Hook k) => k m x -> Hooks m '[k] x

  HookReturn :: Stateful a -> Hooks m '[] a

  HookBind
    :: (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects, Binding a)
    => Hooks m xs a
    -> (Bound a -> Hooks m ys b)
    -> Hooks m (xs ++ ys) b
    -- not a monad :/

{-
-- apologies you have to know so much just to fmap!
instance (effects ~ (effects ++ '[]), Suffix effects '[] (Length effects), Prefix effects effects) => Functor (Hooks m effects) where
  fmap f hook = HookBind hook (HookReturn . f)
-}

stepHooks
  :: forall effects a m
   . (MonadUnliftIO m)
  => (Elem AsyncUpdate effects m -> m ())
  -> Hooks m effects a
  -> Maybe (HookList effects m)
  -> m (a, HookList effects m)
stepHooks writeState (Use programItem) Nothing = do
  (result, item) <- step (writeState . Here) programItem Nothing
  return (result, HookListCons item HookListNil)
stepHooks writeState (Use programItem) (Just (HookListCons listItem rest)) = do
  (result, item) <- step (writeState . Here) programItem (Just listItem)
  return (result, HookListCons item rest)
stepHooks _ (HookReturn (Stateful value)) _ = return (value, HookListNil)
stepHooks writeState (HookBind (program :: Hooks m xs i) combinedFunction) prevList = do
  (a, firstList) <- stepHooks (writeState . prefixStateUpdate) program (hooksListHead <$> prevList)
  (b, secondList) <- stepHooks (writeState . suffixStateUpdate (Proxy @(Length xs))) (combinedFunction $ bind a) (hooksListTail (Proxy @(Length xs)) <$> prevList)
  return (b, hooksListPlusPlus firstList secondList)

applyStateUpdate :: Elem AsyncUpdate effects m -> HookList effects m -> HookList effects m
applyStateUpdate (There updateElem) (HookListCons here rest) = HookListCons here $ applyStateUpdate updateElem rest
applyStateUpdate (Here update) (HookListCons here rest) = HookListCons (updateState update here) rest

data HookExecutionHandle m = HookExecutionHandle
   { await :: m ()
   , terminate :: m ()
   }

runHooks
  :: forall effects a m
   . (MonadUnliftIO m)
  => Hooks m effects a
  -> (a -> m ())
  -> m (HookExecutionHandle m)
runHooks program observe = do
  updateQueue <- newChan
  latestStateRef <- newIORef Nothing
  updateThread <- async $ evolveHook latestStateRef updateQueue Nothing
  return $ HookExecutionHandle
    { await = wait updateThread
    , terminate = do
        cancel updateThread
        mLatestState <- readIORef latestStateRef
        case mLatestState of
          Nothing -> return ()
          Just latestState -> traverseHookList latestState destroy
    }

  where
    evolveHook
      :: IORef (Maybe (HookList effects m))
      -> Chan (Elem AsyncUpdate effects m)
      -> Maybe (HookList effects m) -> m ()
    evolveHook latestStateRef updateQueue state = do
       (result, nextState) <- stepHooks (writeChan updateQueue) program state
       writeIORef latestStateRef (Just nextState)
       observe result
       update <- readChan updateQueue
       evolveHook latestStateRef updateQueue (Just $ applyStateUpdate update nextState)
