{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Hooks.Runtime where

import Control.Monad.Hooks.Class
import Control.Monad.Hooks.List

import Control.Concurrent (readChan, Chan, writeChan, newChan)
import Control.Concurrent.Async (async, wait, cancel)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Proxy (Proxy(..))

data Hooks effects a where
  Use :: (Hook k) => k x -> Hooks '[k] x

  HookReturn :: a -> Hooks '[] a

  HookBind
    :: (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
    => Hooks xs a
    -> (a -> Hooks ys b)
    -> Hooks (xs ++ ys) b
    -- not a monad :/

-- apologies you have to know so much just to fmap!
instance (effects ~ (effects ++ '[]), Suffix effects '[] (Length effects), Prefix effects effects) => Functor (Hooks effects) where
  fmap f hook = HookBind hook (HookReturn . f)

stepHooks
  :: forall effects a
   . (Elem AsyncUpdate effects -> IO ())
  -> Hooks effects a
  -> Maybe (HookList effects)
  -> IO (a, HookList effects)
stepHooks writeState (Use programItem) Nothing = do
  (result, item) <- step (writeState . Here) programItem Nothing
  return (result, HookListCons item HookListNil)
stepHooks writeState (Use programItem) (Just (HookListCons listItem rest)) = do
  (result, item) <- step (writeState . Here) programItem (Just listItem)
  return (result, HookListCons item rest)
stepHooks _ (HookReturn value) _ = return (value, HookListNil)
stepHooks writeState (HookBind (program :: Hooks xs i) combinedFunction) prevList = do
  (a, firstList) <- stepHooks (writeState . prefixStateUpdate) program (hooksListHead <$> prevList)
  (b, secondList) <- stepHooks (writeState . suffixStateUpdate (Proxy @(Length xs))) (combinedFunction a) (hooksListTail (Proxy @(Length xs)) <$> prevList)
  return (b, hooksListPlusPlus firstList secondList)

applyStateUpdate :: Elem AsyncUpdate effects -> HookList effects -> HookList effects
applyStateUpdate (There updateElem) (HookListCons here rest) = HookListCons here $ applyStateUpdate updateElem rest
applyStateUpdate (Here update) (HookListCons here rest) = HookListCons (updateState update here) rest

data HookExecutionHandle = HookExecutionHandle
   { await :: IO ()
   , terminate :: IO ()
   }

runHooks :: forall effects a . Hooks effects a -> (a -> IO ()) -> IO HookExecutionHandle
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
      :: IORef (Maybe (HookList effects))
      -> Chan (Elem AsyncUpdate effects)
      -> Maybe (HookList effects) -> IO ()
    evolveHook latestStateRef updateQueue state = do
       (result, nextState) <- stepHooks (writeChan updateQueue) program state
       writeIORef latestStateRef (Just nextState)
       observe result
       update <- readChan updateQueue
       evolveHook latestStateRef updateQueue (Just $ applyStateUpdate update nextState)
