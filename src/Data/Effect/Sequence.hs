{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, PolyKinds, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Sequence where

import GHC.TypeLits

-- | Non-empty sequences, represented as binary trees.
data Seq a = S a | Seq a :+: Seq a

type S = 'S
type l :+: r = l ':+: r

type family Size (ts :: Seq k) where
  Size ('S _)            = 1
  Size (left ':+: right) = Size left + Size right

infixr 5 :+:

size :: forall tree . KnownNat (Size tree) => Int
size = fromInteger (natVal (undefined :: proxy (Size tree)))


data Side = L | R

type family Find (side :: Maybe Side) (sub :: Seq k) (super :: Seq k) :: Maybe Side where
  Find side sub sub               = side
  Find side sub (left ':+: right) = Find side sub left <> Find side sub right
  Find _    _   _                 = 'Nothing

type family (left :: Maybe k) <> (right :: Maybe k) :: Maybe k where
  'Nothing  <> b         = b
  a         <> 'Nothing  = a
  ('Just _) <> ('Just _) = 'Nothing

type family FromJust (maybe :: Maybe a) :: a where
  FromJust ('Just a) = a


type family Path (sub :: Seq k) (super :: Seq k) :: [Side] where
  Path sub sub              = '[]
  Path sub (left :+: right) = FromJust (Path' 'L sub left <> Path' 'R sub right)

type family Path' (side :: Side) (sub :: Seq k) (super :: Seq k) :: Maybe [Side] where
  Path' side sub sub              = 'Just '[side]
  Path' side sub (left :+: right) = 'Just (side ': FromJust (Path' 'L sub left <> Path' 'R sub right))
  Path' _    _   _                = 'Nothing
