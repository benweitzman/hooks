{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Hooks.Primitives.State where

import Control.Monad.Hooks.Class  
import Control.Monad.Hooks.Runtime (Hooks(Use))
import Data.Functor.Identity

newtype Stateful a = Stateful a
  deriving (Functor, Applicative) via Identity
  deriving newtype Show

data State a m x where
   State :: a -> State a m (Stateful a, AsyncUpdate (State a) m -> m ())
   StateSync :: m a -> State a m (Stateful a, AsyncUpdate (State a) m -> m ())

useState :: a -> Hooks m '[State a] (Stateful a, AsyncUpdate (State a) m -> m ())
useState = Use . State

useStateSync :: m a -> Hooks m '[State a] (Stateful a, AsyncUpdate (State a) m -> m ())
useStateSync = Use . StateSync

instance Hook (State a) where
   data instance HookState (State a) m = StateValue a

   data instance AsyncUpdate (State a) m = Set a | Modify (a -> a)

   updateState (Set a) _ = StateValue a
   updateState (Modify f) (StateValue v) = StateValue (f v)

   step dispatch (StateSync genInitialState) Nothing = do
     initialState <- genInitialState
     return ((Stateful initialState, dispatch), StateValue initialState)
   step dispatch (State initialState) Nothing = return ((Stateful initialState, dispatch), StateValue initialState)
   step dispatch (State _) (Just (StateValue value)) = return ((Stateful value, dispatch), StateValue value)
   step dispatch (StateSync _) (Just (StateValue value)) = return ((Stateful value, dispatch), StateValue value)

   destroy _ = return ()