{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Effect.Union
( Seq
, S
, Empty
, type (:+:)
, Union
, Member
, inject
, project
, weakenSingleton
, strengthenSingleton
, decompose
, Subseq(..)
, type (\\) (..)
) where

import Control.Monad ((<=<))
import Data.Bifunctor
import Data.Effect.Sequence
import Data.Functor.Classes (Show1(..))
import Data.Kind (Type)
import Prelude hiding (splitAt)
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


decompose :: Union (left ':+: right) a -> Either (Union left a) (Union right a)
decompose (Union n member)
  | even n    = Left  (Union (n `div` 2) member)
  | otherwise = Right (Union (n `div` 2) member)


class Subseq sub super where
  weaken     :: Union sub   a ->        Union super a
  strengthen :: Union super a -> Maybe (Union sub   a)

  type family (sub >-> (sub' :: Seq (Type -> Type))) super :: Seq (Type -> Type)
  replace    :: (Union sub a -> Union sub' a) -> Union super a -> Union ((sub >-> sub') super) a
  split      :: proxy sub' -> Union super a -> Either (Union ((sub >-> sub') super) a) (Union sub a)

instance (PathTo sub super ~ path, SubseqAt path sub super) => Subseq sub super where
  weaken     = weakenAt @path
  strengthen = strengthenAt @path

  type (sub >-> sub') super = ReplacedAt (PathTo sub super) sub sub' super
  replace = replaceAt @path
  split   = splitAt @path


class Subseq sub super => (super \\ sub) (difference :: Seq (Type -> Type)) | super sub -> difference where
  delete :: Union super a -> Either (Union difference a) (Union sub a)

instance (PathTo sub super ~ path, DifferenceAt path sub super difference) => (super \\ sub) difference where
  delete = deleteAt @path


class SubseqAt (path :: [Side]) sub super | path super -> sub where
  weakenAt     :: Union sub   a ->        Union super a
  strengthenAt :: Union super a -> Maybe (Union sub   a)

  type family ReplacedAt path sub (sub' :: Seq (Type -> Type)) super :: Seq (Type -> Type)
  replaceAt  :: (Union sub a -> Union sub' a) -> Union super a -> Union (ReplacedAt path sub sub' super) a
  splitAt    :: proxy sub' -> Union super a -> Either (Union (ReplacedAt path sub sub' super) a) (Union sub a)

instance SubseqAt '[] sub sub where
  weakenAt     = id
  strengthenAt = Just

  type ReplacedAt '[] sub sub' sub = sub'
  replaceAt = ($)
  splitAt   = const Right

instance SubseqAt ('L ': '[]) left (left :+: right) where
  weakenAt = weakenLeft
  strengthenAt = either Just (const Nothing) . decompose

  type ReplacedAt ('L ': '[]) left left' (left ':+: right) = left' ':+: right
  replaceAt = replaceLeft
  splitAt _ = splitLeft

instance SubseqAt (next ': rest) sub left => SubseqAt ('L ': next ': rest) sub (left ':+: right) where
  weakenAt     = weakenLeft . weakenAt @(next ': rest)
  strengthenAt = either (strengthenAt @(next ': rest)) (const Nothing) . decompose

  type ReplacedAt ('L ': next ': rest) sub sub' (left ':+: right) = ReplacedAt (next ': rest) sub sub' left ':+: right
  replaceAt = replaceLeft . replaceAt @(next ': rest)
  splitAt p = first weakenLeft . splitAt @(next ': rest) p <=< splitLeft

instance SubseqAt ('R ': '[]) right (left :+: right) where
  weakenAt = weakenRight
  strengthenAt = either (const Nothing) Just . decompose

  type ReplacedAt ('R ': '[]) right right' (left ':+: right) = left ':+: right'
  replaceAt = replaceRight
  splitAt _ = splitRight

instance SubseqAt (next ': rest) sub right => SubseqAt ('R ': next ': rest) sub (left ':+: right) where
  weakenAt     = weakenRight . weakenAt @(next ': rest)
  strengthenAt = either (const Nothing) (strengthenAt @(next ': rest)) . decompose

  type ReplacedAt ('R ': next ': rest) sub sub' (left ':+: right) = left ':+: ReplacedAt (next ': rest) sub sub' right
  replaceAt = replaceRight . replaceAt @(next ': rest)
  splitAt p = first weakenRight . splitAt @(next ': rest) p <=< splitRight


class SubseqAt path sub super => DifferenceAt path sub super (difference :: Seq (Type -> Type)) | path super -> sub difference where
  deleteAt :: Union super a -> Either (Union difference a) (Union sub a)

instance DifferenceAt '[] sub sub Empty where
  deleteAt = Right

instance DifferenceAt ('L ': '[]) left (left :+: right) right where
  deleteAt = either Right Left . decompose

instance DifferenceAt (next ': rest) sub left left' => DifferenceAt ('L ': next ': rest) sub (left :+: right) (left' :+: right) where
  deleteAt = either (first weakenLeft . deleteAt @(next ': rest)) (Left . weakenRight) . decompose

instance DifferenceAt ('R ': '[]) right (left :+: right) left where
  deleteAt = decompose

instance DifferenceAt (next ': rest) sub right right' => DifferenceAt ('R ': next ': rest) sub (left :+: right) (left :+: right') where
  deleteAt = either (Left . weakenLeft) (first weakenRight . deleteAt @(next ': rest)) . decompose


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union (2 * n) t

weakenRight :: Union right a -> Union (left ':+: right) a
weakenRight (Union n t) = Union (1 + (2 * n)) t


replaceLeft :: (Union left a -> Union left' a) -> Union (left ':+: right) a ->  Union (left' ':+: right) a
replaceLeft f = either (weakenLeft . f) weakenRight . decompose

replaceRight :: (Union right a -> Union right' a) -> Union (left ':+: right) a ->  Union (left ':+: right') a
replaceRight f = either weakenLeft (weakenRight . f) . decompose


splitLeft :: Union (left ':+: right) a -> Either (Union (left' ':+: right) a) (Union left a)
splitLeft = either Right (Left . weakenRight) . decompose

splitRight :: Union (left ':+: right) a -> Either (Union (left ':+: right') a) (Union right a)
splitRight = either (Left . weakenLeft) Right . decompose


instance Show (member a) => Show (Union (S member) a) where
  showsPrec d = showsPrec d . strengthenSingleton

instance (Show (Union left a), Show (Union right a)) => Show (Union (left ':+: right) a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance Show1 member => Show1 (Union (S member)) where
  liftShowsPrec sp sl d = liftShowsPrec sp sl d . strengthenSingleton

instance (Show1 (Union left), Show1 (Union right)) => Show1 (Union (left ':+: right)) where
  liftShowsPrec sp sl d = either (liftShowsPrec sp sl d) (liftShowsPrec sp sl d) . decompose
