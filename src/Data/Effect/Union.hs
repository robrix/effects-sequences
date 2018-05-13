{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Union
( Seq(..)
, Union
, Member
, inject
, project
, weakenSingleton
, strengthenSingleton
, decompose
, Subset(..)
) where

import Data.Effect.BinaryTree
import Data.Functor.Classes (Show1(..))
import Data.Kind (Type)
import GHC.TypeLits
import Unsafe.Coerce

data Union (members :: Seq (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)

type Member effect = Subset ('S effect)

inject :: Member effect effects => effect a -> Union effects a
inject = weaken . weakenSingleton

project :: Member effect effects => Union effects a -> Maybe (effect a)
project = fmap strengthenSingleton . strengthen


weakenSingleton :: member a -> Union ('S member) a
weakenSingleton = Union 0

strengthenSingleton :: Union ('S member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member


decompose :: forall left right a . KnownNat (Size left) => Union (left ':+: right) a -> Either (Union left a) (Union right a)
decompose (Union n member)
  | n < left  = Left  (Union n          member)
  | otherwise = Right (Union (n - left) member)
  where left = size @left


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
  strengthenOn = either strengthen (const Nothing) . decompose

instance (Subset sub right, KnownNat (Size left)) => SubsetOn 'R sub (left ':+: right) where
  weakenOn = weakenRight . weaken
  strengthenOn = either (const Nothing) strengthen . decompose


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union n t

weakenRight :: forall left right a . KnownNat (Size left) => Union right a -> Union (left ':+: right) a
weakenRight (Union n t) = Union (size @left + n) t


instance Show (member a) => Show (Union ('S member) a) where
  showsPrec d = showsPrec d . strengthenSingleton

instance (KnownNat (Size left), Show (Union left a), Show (Union right a)) => Show (Union (left ':+: right) a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance Show1 member => Show1 (Union ('S member)) where
  liftShowsPrec sp sl d = liftShowsPrec sp sl d . strengthenSingleton

instance (KnownNat (Size left), Show1 (Union left), Show1 (Union right)) => Show1 (Union (left ':+: right)) where
  liftShowsPrec sp sl d = either (liftShowsPrec sp sl d) (liftShowsPrec sp sl d) . decompose
