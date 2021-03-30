{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Hooks.Stateful where

import Data.Functor.Identity

newtype Stateful a = Stateful a
  deriving (Functor, Applicative, Monad) via Identity
  deriving newtype Show

newtype Escaping a = Escaping a
data AndEscaping a b = AndEscaping a b

type family Bound a where
  Bound (a `AndEscaping` b) = (Stateful a, b)
  Bound (Escaping b) = b
  Bound a = Stateful a

class Binding a where
  bind :: a -> Bound a

instance {-# OVERLAPPING #-} Binding (Escaping a) where
  bind (Escaping a) = a

instance {-# OVERLAPPING #-} Binding (a `AndEscaping` b) where
  bind (a `AndEscaping` b) = (Stateful a, b)

instance (Bound a ~ Stateful a) => Binding a where
  bind = Stateful