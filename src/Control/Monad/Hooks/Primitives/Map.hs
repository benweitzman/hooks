{-# LANGUAGE InstanceSigs #-}

module Control.Monad.Hooks.Primitives.Map where

import Control.Monad.Hooks.Runtime
import Control.Monad.Hooks.Class
import qualified Data.Map as M
import Control.Monad (forM, forM_)
import qualified Data.Set as S
import UnliftIO
import Control.Monad.Hooks.Primitives.State
import Control.Monad.Hooks.List (HookList, Elem, traverseHookList)

data Map effects a b m x where
  Map
    :: Traversable t
    => Stateful (t a)
    -> (a -> Hooks m effects b)
    -> Map effects a b m (t b)

useMap
  :: (Ord a, Traversable t)
  => Stateful (t a)
  -> (a -> Hooks m effects b)
  -> Hooks m '[Map effects a b] (t b)
useMap vals action = Use $ Map vals action

instance (Ord a) => Hook (Map effects a b) where
   data instance HookState (Map effects a b) m = MapItem
      { substates :: M.Map a (HookList effects m)
      }

   data instance AsyncUpdate (Map effects a b) m = MapUpdate a (Elem AsyncUpdate effects m)

   updateState (MapUpdate key update) item@MapItem { substates } = item
     { substates = M.adjust (applyStateUpdate update) key substates
     }

   step
     :: forall m x
      . (MonadUnliftIO m)
     => (AsyncUpdate (Map effects a b) m -> m ())
     -> Map effects a b m x
     -> Maybe (HookState (Map effects a b) m)
     -> m (x, HookState (Map effects a b) m)
   step dispatch (Map (Stateful values) action) mPrevState = do
     results <- forM values $ \value -> do
       let prevSubstate = mPrevState >>= M.lookup value . substates
       substate <- stepHooks (dispatch . MapUpdate value) (action value) prevSubstate
       return (value, substate)
     case mPrevState of
       Just (MapItem prevValues) -> do
        let previous = prevValues `M.withoutKeys` S.fromList (fst <$> toList results)
        forM_ (M.elems previous) $ \substate -> traverseHookList substate destroy
       Nothing -> return ()
     return (fst . snd <$> results, MapItem $ fmap snd $ M.fromList $ toList results)

     where

       toList = foldMap (:[])

   destroy (MapItem substates) = forM_ (M.elems substates) $ \substate -> traverseHookList substate destroy