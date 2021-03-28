{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Hooks.Stateful where

import Data.Functor.Identity

newtype Stateful a = Stateful a
  deriving (Functor, Applicative, Monad) via Identity
  deriving newtype Show

data Escaping a b = Escaping a b

type family Bound a where
  Bound (a `Escaping` b) = (Stateful a, b)
  Bound a = Stateful a

class Binding a where
  bind :: a -> Bound a

instance {-# OVERLAPPING #-} Binding (a `Escaping` b) where
  bind (a `Escaping` b) = (Stateful a, b)

instance (Bound a ~ Stateful a) => Binding a where
  bind = Stateful