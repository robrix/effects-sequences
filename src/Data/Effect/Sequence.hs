{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Sequence where

-- | Sequences, represented as binary trees.
data Seq a = Empty | S a | Seq a :+: Seq a

type Empty = 'Empty
type S = 'S
type l :+: r = l ':+: r

infixr 5 :+:


data Side = L | R

type family (left :: Maybe k) <> (right :: Maybe k) :: Maybe k where
  'Nothing  <> b         = b
  a         <> 'Nothing  = a
  ('Just _) <> ('Just _) = 'Nothing

type family FromJust (maybe :: Maybe a) :: a where
  FromJust ('Just a) = a


type family PathTo (sub :: Seq k) (super :: Seq k) :: [Side] where
  PathTo sub sub              = '[]
  PathTo sub (left :+: right) = FromJust (PathTo' 'L sub left <> PathTo' 'R sub right)

type family PathTo' (side :: Side) (sub :: Seq k) (super :: Seq k) :: Maybe [Side] where
  PathTo' side sub sub              = 'Just '[side]
  PathTo' side sub (left :+: right) = 'Just (side ': FromJust (PathTo' 'L sub left <> PathTo' 'R sub right))
  PathTo' _    _   _                = 'Nothing
