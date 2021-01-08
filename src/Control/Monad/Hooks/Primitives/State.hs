module Control.Monad.Hooks.Primitives.State where

import Control.Monad.Hooks.Class  
  
data State a x where
   State :: a -> State a (a, AsyncUpdate (State a) -> IO ())

instance Hook (State a) where
   data instance HookState (State a) = StateValue a

   data instance AsyncUpdate (State a) = Set a | Modify (a -> a)

   updateState (Set a) _ = StateValue a
   updateState (Modify f) (StateValue v) = StateValue (f v)

   step dispatch (State initialState) Nothing = return ((initialState, dispatch), StateValue initialState)
   step dispatch (State _) (Just (StateValue value)) = return ((value, dispatch), StateValue value)

   destroy _ = return ()