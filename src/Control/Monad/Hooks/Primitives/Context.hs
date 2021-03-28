module Control.Monad.Hooks.Primitives.Context where

import Control.Monad.Hooks.Class
import GHC.TypeLits (Symbol)
import Control.Monad.Hooks.Runtime (Hooks(Use), stepHooks, applyStateUpdate)
import UnliftIO hiding (handle)
import Control.Monad.Hooks.List (HookList, traverseHookList, Elem)
import Type.Reflection (TypeRep, (:~~:)(HRefl), eqTypeRep, typeRep)

data Context (s :: Symbol) m x where
  Context :: forall s a m effects . Typeable effects => Hooks m effects a -> Context s m a

typeOfHooks :: Typeable effects => Hooks m effects a -> TypeRep effects
typeOfHooks _ = typeRep

useContext :: forall c a m effects . Typeable effects => Hooks m effects a -> Hooks m '[Context c] a
useContext hook = Use $ Context hook

instance Hook (Context c) where
  data instance HookState (Context c) m where
    HiddenState :: TypeRep effects -> HookList effects m -> HookState (Context c) m

  data instance AsyncUpdate (Context c) m where
    HiddenUpdate :: TypeRep effects -> Elem AsyncUpdate effects m -> AsyncUpdate (Context c) m

  updateState (HiddenUpdate rep1 update) (HiddenState rep2 state) = case eqTypeRep rep1 rep2 of
    Just HRefl -> HiddenState rep1 $ applyStateUpdate update state

  destroy (HiddenState _ substate) = traverseHookList substate destroy

  step dispatch (Context hidden) mPrevState = case mPrevState of
    Nothing -> do
      (a, nextState) <- stepHooks (dispatch . HiddenUpdate typeRep) hidden Nothing
      return (a, HiddenState typeRep nextState)
    Just (HiddenState rep1 substate) -> case eqTypeRep rep1 (typeOfHooks hidden)  of
      Just HRefl -> do
        (a, nextState) <- stepHooks (dispatch . HiddenUpdate typeRep) hidden (Just substate)
        return (a, HiddenState typeRep nextState)