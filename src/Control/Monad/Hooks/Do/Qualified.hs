module Control.Monad.Hooks.Do.Qualified where

import qualified Prelude as P
import Control.Monad.Hooks
import Control.Monad.Hooks.List

return :: forall a . a -> Hooks '[] a
return = HookReturn

(>>=)
  :: forall effects xs ys a b
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
  => Hooks xs a
  -> (a -> Hooks ys b)
  -> Hooks effects b
(>>=) = HookBind


(>>)
  :: forall effects xs ys a b
  . (Prefix effects xs, Suffix effects ys (Length xs), (xs ++ ys) ~ effects)
  => Hooks xs a
  -> Hooks ys b
  -> Hooks effects b
x >> y = x >>= P.const y

