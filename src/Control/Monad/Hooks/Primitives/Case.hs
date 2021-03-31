{-# LANGUAGE InstanceSigs #-}
module Control.Monad.Hooks.Primitives.Case where

import Control.Monad.Hooks.Class
import Control.Monad.Hooks.Runtime
import Data.Kind (Type)
import Control.Monad.Hooks.Stateful (Stateful(Stateful))
import Control.Monad.Hooks.List (Elem(Here, There), HookList, traverseHookList)
import UnliftIO
import Control.Monad (forM_)

data Clause m a b effects where
  When :: (a -> Bool) -> Hooks m effects b -> Clause m a b effects
  LetWhen :: (a -> Maybe c) -> (c -> Hooks m effects b) -> Clause m a b effects

(->>) :: (a -> Bool) -> Hooks m effects b -> Clause m a b effects
(->>) = When

(->>*) :: (a -> Maybe c) -> (c -> Hooks m effects b) -> Clause m a b effects
(->>*) = LetWhen

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

  -- this one's a bit of a doozy. the big picture idea is that we keep track
  -- of one case branch's state at a time. if we stay on a branch between steps,
  -- we evolve the branch's state. otherwise we have to create a new one and clean
  -- up the old one.
  step d (Case (Stateful val) c) mp = go (d . CaseUpdate) c mp
    where
      -- we have to traverse a few type level lists
      -- in parallel in order to evaluate different branchs. We'll introduce a helper
      -- function to keep track of the recursion levels since our class function is
      -- pinned to the top level.
      go
        :: MonadUnliftIO m
        => (Elem (Elem AsyncUpdate) es m -> m ())
        -> Clauses m a b es
        -> Maybe (HookState (Case a b es) m)
        -> m (b, HookState (Case a b es) m)
      -- if we find ourselves on the Else branch, life is easy
      -- Else branches don't have state, so we only need to destroy
      -- the previous state if one existed. Doesn't matter which branch it was
      go _ (Else b) mPrevState = do
        forM_ mPrevState destroy
        return (b, ElseCaseState)
      -- WhenBranches are really just a special cases of LetWhen that doesn't
      -- bind any values:
      --  (a -> Bool) ~ (a -> Maybe ())
      --
      -- so we can rewrite before evaluating
      go dispatch (Or (When match body) clauses) mPrevState
        | match val = go dispatch (Or (LetWhen (const $ Just ()) (const body)) clauses) mPrevState
        | otherwise = go dispatch (Or (LetWhen (const Nothing) (const body)) clauses) mPrevState

      -- and finally we have to start doing something interesting. We'll start off
      -- by testing our condition to see if this branch is going to apply or not
      go dispatch (Or (LetWhen match body) clauses) mPrevState = case match val of
        -- the branch applies and we'll keep track of the value bound by the
        -- "pattern" so it can passed to the body
        Just bind -> do
          -- we extract a "useful previous state". If we encounter a previous
          -- state that belongs to another branch, it's not useful and we'll
          -- destroy it
          mPrevState' <- case mPrevState of
            -- no state, nothing to do
            Nothing -> return Nothing
            -- previous state that lines up with our branch, we'll continue using it
            Just (CaseState (Here prevState)) -> return (Just prevState)
            -- if it's not here, then it's there. Doesn't matter where it is, but
            -- we don't want it anymore.
            Just prevState -> do
              destroy prevState
              return Nothing
          (b, state) <- stepHooks (dispatch . Here) (body bind) mPrevState'
          return (b, CaseState $ Here state)

        -- if we're here, then the branch we're currently examining is not going to
        -- apply.
        Nothing -> do
          -- extracting a "useful previous state" is a little different than above.
          -- We have to remove any state associate with this branch (since it's no
          -- longer active), but any state belonging to later branches should stick
          -- around (for now, until we figure out which one applies).
          mPrevState' <- case mPrevState of
            -- no state, nothing to do
            Nothing -> return Nothing
            -- we used to be in this branch, but it's not going to apply anymore
            -- so it can be cleaned up
            Just (CaseState (Here prevState)) -> do
              traverseHookList prevState destroy
              return Nothing
            -- otherwise, we'll want to preserve the state as we recurse, but
            -- peel off a layer
            Just (CaseState (There prevState)) -> do
              return $ Just (CaseState prevState)
            Just ElseCaseState -> return Nothing

          -- since our branch didn't match, we'll recurse to continue trying
          -- other branches
          go (dispatch . There) clauses mPrevState' >>= \case
            (b, ElseCaseState) -> return (b, ElseCaseState)
            (b, CaseState there) -> return (b, CaseState $ There there)
