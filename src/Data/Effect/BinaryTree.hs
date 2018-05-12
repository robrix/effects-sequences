{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, PolyKinds, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.BinaryTree where

import GHC.TypeLits

data BinaryTree a = Z | S a | BinaryTree a :+: BinaryTree a

type family Size (ts :: BinaryTree k) where
  Size 'Z                = 0
  Size ('S _)            = 1
  Size (left ':+: right) = Size left + Size right

size :: forall tree . KnownNat (Size tree) => Int
size = fromInteger (natVal (undefined :: proxy (Size tree)))
