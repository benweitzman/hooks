module Control.Monad.Hooks.Primitives.Case where

import Control.Monad.Hooks.Class
import Control.Monad.Hooks.Runtime
import Data.Kind (Type)
import Control.Monad.Hooks.Stateful (Stateful(Stateful))
import Control.Monad.Hooks.List (Elem(Here, There), HookList, traverseHookList)
import UnliftIO
import Control.Monad (forM_)

{-
useSomthing = Hook.do
  useCase someValue $ Branch.do
    When isEven $ Hook.do
      blah blah
      blah
    When isOdd $ Hook.do
      blah blah
      blah
    End
-}

data Clause m a b effects where
  When :: (a -> Bool) -> Hooks m effects b -> Clause m a b effects
  LetWhen :: (a -> Maybe c) -> (c -> Hooks m effects b) -> Clause m a b effects

data Clauses (m :: Type -> Type) (a :: Type) (b :: Type) (bs :: [[HookK]]) where
  Or :: Clause m a b x -> Clauses m a b ys -> Clauses m a b (x ': ys)
  Else :: b -> Clauses m a b '[]

data Case a b (clauses :: [[HookK]]) m x where
  Case :: Stateful a -> Clauses m a b clauses -> Case a b clauses m b

useCase :: Stateful a -> Clauses m a x clauses -> Hooks m '[Case a x clauses] x
useCase input clauses = Use $ Case input clauses

instance Hook (Case a b clauses) where
  data instance HookState (Case a b clauses) m where
    CaseState :: Elem HookList clauses m -> HookState (Case a b clauses) m
    ElseCaseState :: HookState (Case a b clauses) m

  data instance AsyncUpdate (Case a b clauses) m = CaseUpdate (Elem (Elem AsyncUpdate) clauses m)

  updateState _ ElseCaseState = ElseCaseState
  updateState (CaseUpdate u) (CaseState s) = CaseState $ go u s
    where
      go :: Elem (Elem AsyncUpdate) xs m -> Elem HookList xs m -> Elem HookList xs m
      go (Here update) (Here state) = Here $ applyStateUpdate update state
      go (There update) (There state) = There $ go update state
      go _ noop = noop

  destroy ElseCaseState = return ()
  destroy (CaseState s) = go s
    where
      go :: Monad m => Elem HookList xs m -> m ()
      go (Here state) = traverseHookList state destroy
      go (There state) = go state

  step d (Case (Stateful val) c) mp = go (d . CaseUpdate) c mp
    where
      go
        :: MonadUnliftIO m
        => (Elem (Elem AsyncUpdate) es m -> m ())
        -> Clauses m a b es
        -> Maybe (HookState (Case a b es) m)
        -> m (b, HookState (Case a b es) m)
      go _ (Else b) mPrevState = do
        forM_ mPrevState destroy
        return (b, ElseCaseState)
      go dispatch (Or (When match body) clauses) mPrevState
        | match val = go dispatch (Or (LetWhen (const $ Just ()) (const body)) clauses) mPrevState
        | otherwise = go dispatch (Or (LetWhen (const Nothing) (const body)) clauses) mPrevState
      go dispatch (Or (LetWhen match body) clauses) mPrevState = case match val of
        Just bind -> do
          mPrevState' <- case mPrevState of
            Nothing -> return Nothing
            Just (CaseState (Here prevState)) -> return (Just prevState)
            Just prevState -> do
              destroy prevState
              return Nothing
          (b, state) <- stepHooks (dispatch . Here) (body bind) mPrevState'
          return (b, CaseState $ Here state)
        Nothing -> do
          mPrevState' <- case mPrevState of
            Nothing -> return Nothing
            Just (CaseState (Here prevState)) -> do
              traverseHookList prevState destroy
              return Nothing
            Just (CaseState (There prevState)) -> do
              return $ Just (CaseState prevState)
            Just ElseCaseState -> return Nothing
          go (dispatch . There) clauses mPrevState' >>= \case
            (b, ElseCaseState) -> return (b, ElseCaseState)
            (b, CaseState there) -> return (b, CaseState $ There there)
