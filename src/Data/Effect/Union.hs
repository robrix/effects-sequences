{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Union
( Seq
, S
, type (:+:)
, Union
, Member
, inject
, project
, weakenSingleton
, strengthenSingleton
, decompose
, Subseq(..)
, KnownNat
, Size
) where

import Data.Effect.Sequence
import Data.Functor.Classes (Show1(..))
import Data.Kind (Type)
import GHC.TypeLits
import Unsafe.Coerce

data Union (members :: Seq (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)

type Member member = Subseq (S member)

inject :: Member member members => member a -> Union members a
inject = weaken . weakenSingleton

project :: Member member members => Union members a -> Maybe (member a)
project = fmap strengthenSingleton . strengthen


weakenSingleton :: member a -> Union (S member) a
weakenSingleton = Union 0

strengthenSingleton :: Union (S member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member


decompose :: forall left right a . KnownNat (Size left) => Union (left ':+: right) a -> Either (Union left a) (Union right a)
decompose (Union n member)
  | n < left  = Left  (Union n          member)
  | otherwise = Right (Union (n - left) member)
  where left = size @left


class Subseq sub super where
  weaken     :: Union sub   a ->        Union super a
  strengthen :: Union super a -> Maybe (Union sub   a)

instance (Path sub super ~ path, SubseqAt path sub super) => Subseq sub super where
  weaken = weakenAt @path
  strengthen = strengthenAt @path


class SubseqAt (side :: [Side]) sub super where
  weakenAt :: Union sub a -> Union super a
  strengthenAt :: Union super a -> Maybe (Union sub a)

instance SubseqAt '[] sub sub where
  weakenAt     = id
  strengthenAt = Just

instance (SubseqAt rest sub left, KnownNat (Size left)) => SubseqAt ('L ': rest) sub (left ':+: right) where
  weakenAt     = weakenLeft . weakenAt @rest
  strengthenAt = either (strengthenAt @rest) (const Nothing) . decompose

instance (SubseqAt rest sub right, KnownNat (Size left)) => SubseqAt ('R ': rest) sub (left ':+: right) where
  weakenAt     = weakenRight . weakenAt @rest
  strengthenAt = either (const Nothing) (strengthenAt @rest) . decompose


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union n t

weakenRight :: forall left right a . KnownNat (Size left) => Union right a -> Union (left ':+: right) a
weakenRight (Union n t) = Union (size @left + n) t


instance Show (member a) => Show (Union (S member) a) where
  showsPrec d = showsPrec d . strengthenSingleton

instance (KnownNat (Size left), Show (Union left a), Show (Union right a)) => Show (Union (left ':+: right) a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance Show1 member => Show1 (Union (S member)) where
  liftShowsPrec sp sl d = liftShowsPrec sp sl d . strengthenSingleton

instance (KnownNat (Size left), Show1 (Union left), Show1 (Union right)) => Show1 (Union (left ':+: right)) where
  liftShowsPrec sp sl d = either (liftShowsPrec sp sl d) (liftShowsPrec sp sl d) . decompose
