{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, PolyKinds, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.BinaryTree where

import GHC.TypeLits

data Set a = Z | S a | Set a :+: Set a

type family Size (ts :: Set k) where
  Size 'Z                = 0
  Size ('S _)            = 1
  Size (left ':+: right) = Size left + Size right

size :: forall tree . KnownNat (Size tree) => Int
size = fromInteger (natVal (undefined :: proxy (Size tree)))


data Side = L | R

type family Find (side :: Maybe Side) (sub :: Set k) (super :: Set k) :: Maybe Side where
  Find side sub sub               = side
  Find side sub (left ':+: right) = Find side sub left <> Find side sub right
  Find _    _   _                 = 'Nothing

type family (left :: Maybe Side) <> (right :: Maybe Side) :: Maybe Side where
  'Nothing  <> b         = b
  a         <> 'Nothing  = a
  ('Just _) <> ('Just _) = 'Nothing

type family FromJust (maybe :: Maybe a) :: a where
  FromJust ('Just a) = a
