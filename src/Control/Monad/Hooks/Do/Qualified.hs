module Control.Monad.Hooks.Do.Qualified where

import qualified Prelude as P
import Control.Monad.Hooks
import Control.Monad.Hooks.List

return :: forall a m . a -> Hooks m '[] a
return = HookReturn

(>>=)
  :: forall effects xs ys a b m
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
  => Hooks m xs a
  -> (a -> Hooks m ys b)
  -> Hooks m effects b
(>>=) = HookBind


(>>)
  :: forall effects xs ys a b m
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
  => Hooks m xs a
  -> Hooks m ys b
  -> Hooks m effects b
x >> y = x >>= P.const y

