module Control.Monad.Hooks.Primitives.State where

import Control.Monad.Hooks.Class  
  
data State a m x where
   State :: a -> State a m (a, AsyncUpdate (State a) m -> m ())
   StateSync :: m a -> State a m (a, AsyncUpdate (State a) m -> m ())

instance Hook (State a) where
   data instance HookState (State a) m = StateValue a

   data instance AsyncUpdate (State a) m = Set a | Modify (a -> a)

   updateState (Set a) _ = StateValue a
   updateState (Modify f) (StateValue v) = StateValue (f v)

   step dispatch (StateSync genInitialState) Nothing = do
     initialState <- genInitialState
     return ((initialState, dispatch), StateValue initialState)
   step dispatch (State initialState) Nothing = return ((initialState, dispatch), StateValue initialState)
   step dispatch (State _) (Just (StateValue value)) = return ((value, dispatch), StateValue value)
   step dispatch (StateSync _) (Just (StateValue value)) = return ((value, dispatch), StateValue value)

   destroy _ = return ()