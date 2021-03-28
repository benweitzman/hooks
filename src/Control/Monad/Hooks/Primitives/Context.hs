module Control.Monad.Hooks.Primitives.Context where

import Control.Monad.Hooks.Class
import GHC.TypeLits (Symbol)
import Control.Monad.Hooks.Runtime (Hooks(Use), stepHooks, applyStateUpdate)
import Control.Monad.Hooks.List (HookList, traverseHookList, Elem)
import Unsafe.Coerce (unsafeCoerce)

data Context (s :: Symbol) m x where
  Context :: forall s a m effects . Hooks m effects a -> Context s m a

useContext :: forall c a m effects . Hooks m effects a -> Hooks m '[Context c] a
useContext hook = Use $ Context hook

instance Hook (Context c) where
  data instance HookState (Context c) m where
    HiddenState :: HookList effects m -> HookState (Context c) m

  data instance AsyncUpdate (Context c) m where
    HiddenUpdate :: Elem AsyncUpdate effects m -> AsyncUpdate (Context c) m

  updateState (HiddenUpdate update) (HiddenState state) = HiddenState $ applyStateUpdate update $ unsafeCoerce state

  destroy (HiddenState substate) = traverseHookList substate destroy

  step dispatch (Context hidden) mPrevState = do
    let coercedPrevState = case mPrevState of
          Nothing -> Nothing
          Just (HiddenState substate) -> Just $ unsafeCoerce substate
    (a, nextState) <- stepHooks (dispatch . HiddenUpdate) hidden coercedPrevState
    return (a, HiddenState nextState)