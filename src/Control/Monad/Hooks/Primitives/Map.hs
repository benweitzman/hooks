module Control.Monad.Hooks.Primitives.Map where

import Control.Monad.Hooks.Runtime
import Control.Monad.Hooks.Class
import qualified Data.Map as M
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (forM, forM_)
import qualified Data.Set as S

data Map deps a b x where
  Map :: deps -> [a] -> (a -> Hooks effects b) -> Map deps a b [b]

instance (Ord a, Eq deps) => Hook (Map deps a b) where
   data instance HookState (Map deps a b) = MapItem
      { dependencies :: deps
      , values :: M.Map a (b, HookExecutionHandle)
      }

   data instance AsyncUpdate (Map deps a b) = MapUpdate a b

   updateState (MapUpdate key value) item@MapItem { values } = item
     { values = M.adjust (\(_, handle) -> (value, handle)) key values
     }

   step dispatch (Map deps values action) mPrevState = case mPrevState of
     Nothing -> do
       results <- forM values $ \value -> (value,) <$> createSubcontext value
       return (fst . snd <$> results, MapItem deps (M.fromList results))
     Just (MapItem prevDependencies prevValues) -> do
       let depsChanged = prevDependencies /= deps
       results <- forM values $ \value -> do
         let mPrevValue = M.lookup value prevValues
         case mPrevValue of
           Just prevValue | not depsChanged -> return (value, prevValue)
           _ -> (value,) <$> createSubcontext value
       let previous = prevValues `M.withoutKeys` S.fromList (fst <$> results)
       forM_ (M.elems previous) $ \(_, HookExecutionHandle { terminate } ) -> terminate
       return (fst . snd <$> results, MapItem deps (M.fromList results))


     where

       createSubcontext :: a -> IO (b, HookExecutionHandle)
       createSubcontext value = do
         mvar <- newEmptyMVar
         initialRef <- newIORef True
         handle <- runHooks (action value) $ \result -> do
           isInitial <- readIORef initialRef
           if isInitial
           then putMVar mvar result >> writeIORef initialRef False
           else dispatch $ MapUpdate value result
         initialValue <- readMVar mvar
         return (initialValue, handle)

   destroy (MapItem _ values) = forM_ (M.elems values) $ \(_, HookExecutionHandle { terminate } ) -> terminate