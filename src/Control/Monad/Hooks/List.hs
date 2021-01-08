{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Hooks.List where

import Control.Monad.Hooks.Class
import Data.Proxy (Proxy(..))

data Elem f effects where
  Here :: f a -> Elem f (a ':  effects)
  There :: Elem f effects -> Elem f (e ': effects)

data HookList effects where
   HookListNil :: HookList '[]
   HookListCons :: Hook k => HookState k -> HookList effects -> HookList (k ': effects)

traverseHookList :: Monad m => HookList effects -> (forall e . Hook e => HookState e -> m ()) -> m ()
traverseHookList HookListNil _ = return ()
traverseHookList (HookListCons item rest) apply = do
  apply item
  traverseHookList rest apply

hooksListPlusPlus :: HookList xs -> HookList ys -> HookList (xs ++ ys)
hooksListPlusPlus HookListNil ys = ys
hooksListPlusPlus (HookListCons item rest) ys = HookListCons item (hooksListPlusPlus rest ys)

data N = Z | S N

class Suffix effects xs drop | effects drop -> xs where
  hooksListTail :: Proxy drop -> HookList effects -> HookList xs

  suffixStateUpdate :: Proxy drop -> Elem f xs -> Elem f effects

instance (effects ~ effects') => Suffix effects effects' 'Z where
  hooksListTail _ = id

  suffixStateUpdate _ = id

instance (Suffix effects ys drop) => Suffix (e ': effects) ys ('S drop)where
  hooksListTail _ (HookListCons _ rest) = hooksListTail (Proxy @drop) rest

  suffixStateUpdate _ x = There $ suffixStateUpdate (Proxy @drop) x

class Prefix effects xs where
  hooksListHead :: HookList effects -> HookList xs

  prefixStateUpdate :: Elem f xs -> Elem f effects

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