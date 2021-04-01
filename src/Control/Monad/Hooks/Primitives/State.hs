module Control.Monad.Hooks.Primitives.State where

import Control.Monad.Hooks.Stateful
import Control.Monad.Hooks.Class  
import Control.Monad.Hooks.Runtime (Hooks(Use))

data State a m x where
   State :: a -> State a m (a `AndEscaping` (StateUpdate a -> m ()))
   StateSync :: m a -> State a m (a `AndEscaping` (StateUpdate a -> m ()))

data StateUpdate a = Set a | Modify (a -> a)

-- | `useState` declares a state variable, providing access to the stateful value inside
-- and a way to imperatively update the value. It takes as an argument the initial value
-- for the state.
--
-- The update function return is not itself stateful, so it's said to "escape". It can
-- be safely referenced and passed around.
useState :: a -> Hooks m '[State a] (a `AndEscaping` (StateUpdate a -> m ()))
useState = Use . State

-- | `useStateSync` declares a state variable, providing access to the stateful value inside
-- and a way to imperatively update the value. It takes as an argument an impure action that will
-- synchronously be called to fetch the initial state. 
--
-- The update function return is not itself stateful, so it's said to "escape". It can
-- be safely referenced and passed around.
useStateSync :: m a -> Hooks m '[State a] (a `AndEscaping` (StateUpdate a -> m ()))
useStateSync = Use . StateSync

instance Hook (State a) where
   data instance HookState (State a) m = StateValue a

   data instance AsyncUpdate (State a) m = StateUpdateOf (StateUpdate a)

   updateState (StateUpdateOf (Set a)) _ = StateValue a
   updateState (StateUpdateOf (Modify f)) (StateValue v) = StateValue (f v)

   step dispatch (StateSync genInitialState) Nothing = do
     initialState <- genInitialState
     return (initialState `AndEscaping` (dispatch . StateUpdateOf), StateValue initialState)
   step dispatch (State initialState) Nothing = return (initialState `AndEscaping` (dispatch . StateUpdateOf), StateValue initialState)
   step dispatch (State _) (Just (StateValue value)) = return (value `AndEscaping` (dispatch . StateUpdateOf), StateValue value)
   step dispatch (StateSync _) (Just (StateValue value)) = return (value `AndEscaping` (dispatch . StateUpdateOf), StateValue value)

   destroy _ = return ()