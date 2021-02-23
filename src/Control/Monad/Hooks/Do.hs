module Control.Monad.Hooks.Do where

import Control.Monad.Hooks hiding (Hook)
import Control.Monad.Hooks.List
import Prelude hiding (return, (>>=), (>>), Monad)
import qualified Prelude as P

data Hook  = Hook
  { return :: forall a m . a -> Hooks m '[] a
  , (>>=)
    :: forall effects xs ys a b m
    . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
    => Hooks m xs a
    -> (a -> Hooks m ys b)
    -> Hooks m effects b
  , (>>)
        :: forall effects xs ys a b m
        . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
        => Hooks m xs a
        -> Hooks m ys b
        -> Hooks m effects b
  }

hook :: Hook
hook = Hook
  { return = HookReturn
  , (>>=) = HookBind
  , (>>) = \x y -> x `HookBind` const y
  }

data Monad = Monad
  { return :: forall a m. (P.Monad m) => a -> m a
  , (>>=) :: forall a b m. (P.Monad m) =>  m a -> (a -> m b) -> m b
  , (>>) :: forall a b m. (P.Monad m) =>  m a -> m b -> m b
  }

monad :: Monad
monad = Monad
  { return = P.return
  , (>>=) = (P.>>=)
  , (>>) = (P.>>)
  }
