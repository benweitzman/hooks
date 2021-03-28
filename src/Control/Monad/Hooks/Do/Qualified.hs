module Control.Monad.Hooks.Do.Qualified where

import qualified Prelude as P
import Control.Monad.Hooks
import Control.Monad.Hooks.List
import Control.Monad.Hooks.Stateful

return :: forall a m . Stateful a -> Hooks m '[] a
return = HookReturn

(>>=)
  :: forall effects xs ys a b m
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects, Binding a)
  => Hooks m xs a
  -> (Bound a -> Hooks m ys b)
  -> Hooks m effects b
(>>=) = HookBind

(>>)
  :: forall effects xs ys a b m
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects, Binding a)
  => Hooks m xs a
  -> Hooks m ys b
  -> Hooks m effects b
x >> y = x >>= P.const y