{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Hooks.List where

import Control.Monad.Hooks.Class
import Data.Proxy (Proxy(..))

data Elem f effects m where
  Here :: f a m -> Elem f (a ':  effects) m
  There :: Elem f effects m -> Elem f (e ': effects) m

data HookList effects m where
   HookListNil :: HookList '[] m
   HookListCons :: Hook k => HookState k m -> HookList effects m -> HookList (k ': effects) m

traverseHookList :: Monad m => HookList effects n -> (forall e . Hook e => HookState e n -> m ()) -> m ()
traverseHookList HookListNil _ = return ()
traverseHookList (HookListCons item rest) apply = do
  apply item
  traverseHookList rest apply

hooksListPlusPlus :: HookList xs m -> HookList ys m -> HookList (xs ++ ys) m
hooksListPlusPlus HookListNil ys = ys
hooksListPlusPlus (HookListCons item rest) ys = HookListCons item (hooksListPlusPlus rest ys)

data N = Z | S N

class Suffix effects xs drop | effects drop -> xs where
  hooksListTail :: Proxy drop -> HookList effects m -> HookList xs m

  suffixStateUpdate :: Proxy drop -> Elem f xs m -> Elem f effects m

instance (effects ~ effects') => Suffix effects effects' 'Z where
  hooksListTail _ = id

  suffixStateUpdate _ = id

instance (Suffix effects ys drop) => Suffix (e ': effects) ys ('S drop)where
  hooksListTail _ (HookListCons _ rest) = hooksListTail (Proxy @drop) rest

  suffixStateUpdate _ x = There $ suffixStateUpdate (Proxy @drop) x

class Prefix effects xs where
  hooksListHead :: HookList effects m -> HookList xs m

  prefixStateUpdate :: Elem f xs m -> Elem f effects m

instance Prefix effects '[] where
  hooksListHead _ = HookListNil

  prefixStateUpdate x = case x of {}

instance Prefix effects xs => Prefix (e ': effects) (e ': xs) where
  hooksListHead (HookListCons item rest) = HookListCons item $ hooksListHead rest

  prefixStateUpdate (Here update) = Here update
  prefixStateUpdate (There xs) = There $ prefixStateUpdate xs

type family (xs :: [k]) ++ (ys :: [k]) where
  (x ': xs) ++ ys = x ': (xs ++ ys)
  '[] ++ ys = ys

type family Length (xs :: [k]) where
  Length '[] = 'Z
  Length (x ': xs) = 'S (Length xs)