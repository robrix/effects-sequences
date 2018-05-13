{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Union
( Set(..)
, Union
, Member
, inject
, project
, weakenSingleton
, strengthenSingleton
, Subset(..)
) where

import Control.Monad ((<=<))
import Data.Effect.BinaryTree
import Data.Functor.Classes (showsBinaryWith)
import Data.Kind (Type)
import GHC.TypeLits
import Unsafe.Coerce

data Union (members :: Set (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)

type Member effect = Subset ('S effect)

inject :: Member effect effects => effect a -> Union effects a
inject = weaken . weakenSingleton

project :: Member effect effects => Union effects a -> Maybe (effect a)
project = fmap strengthenSingleton . strengthen


weakenSingleton :: member a -> Union ('S member) a
weakenSingleton = Union 0

strengthenSingleton :: Union ('S member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member


class Subset sub super where
  weaken :: Union sub a -> Union super a
  strengthen :: Union super a -> Maybe (Union sub a)

instance Subset ('S member) ('S member) where
  weaken = id
  strengthen = Just

instance Subset (left ':+: right) (left ':+: right) where
  weaken = id
  strengthen = Just

instance (FromJust (Find ('Just 'L) sub left <> Find ('Just 'R) sub right) ~ side, SubsetOn side sub (left ':+: right)) => Subset sub (left ':+: right) where
  weaken = weakenOn @side
  strengthen = strengthenOn @side


class SubsetOn (side :: Side) sub super where
  weakenOn :: Union sub a -> Union super a
  strengthenOn :: Union super a -> Maybe (Union sub a)

instance (Subset sub left, KnownNat (Size left)) => SubsetOn 'L sub (left ':+: right) where
  weakenOn = weakenLeft . weaken
  strengthenOn = strengthen <=< strengthenLeft

instance (Subset sub right, KnownNat (Size left)) => SubsetOn 'R sub (left ':+: right) where
  weakenOn = weakenRight . weaken
  strengthenOn = strengthen <=< strengthenRight


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union n t

weakenRight :: forall left right a . KnownNat (Size left) => Union right a -> Union (left ':+: right) a
weakenRight (Union n t) = Union (size @left + n) t


strengthenLeft :: forall left right a . KnownNat (Size left) => Union (left ':+: right) a -> Maybe (Union left a)
strengthenLeft (Union n member)
  | n < size @left = Just (Union n member)
  | otherwise      = Nothing

strengthenRight :: forall left right a . KnownNat (Size left) => Union (left ':+: right) a -> Maybe (Union right a)
strengthenRight (Union n member)
  | let left  = size @left
  , n >= left = Just (Union (n - left) member)
  | otherwise = Nothing


instance Show (member a) => Show (Union ('S member) a) where
  showsPrec d u@(Union n _) = showsBinaryWith showsPrec showsPrec "Union" d n (strengthenSingleton u)
