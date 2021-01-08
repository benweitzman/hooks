module Control.Monad.Hooks.Do where

import Control.Monad.Hooks hiding (Hook)
import Control.Monad.Hooks.List
import Prelude hiding (return, (>>=), (>>), Monad)
import qualified Prelude as P

data Hook  = Hook
  { return :: forall a . a -> Hooks '[] a
  , (>>=)
    :: forall effects xs ys a b
    . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
    => Hooks xs a
    -> (a -> Hooks ys b)
    -> Hooks effects b
  , (>>)
        :: forall effects xs ys a b
        . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
        => Hooks xs a
        -> Hooks ys b
        -> Hooks effects b
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
