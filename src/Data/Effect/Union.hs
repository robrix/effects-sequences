{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
, ProperSubseq(..)
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

instance SubseqAt rest sub left => SubseqAt ('L ': rest) sub (left ':+: right) where
  weakenAt     = weakenLeft . weakenAt @rest
  strengthenAt = either (strengthenAt @rest) (const Nothing) . decompose

  type ReplacedAt ('L ': rest) sub sub' (left ':+: right) = ReplacedAt rest sub sub' left ':+: right
  replaceAt = replaceLeft . replaceAt @rest
  splitAt p = first weakenLeft . splitAt @rest p <=< splitLeft

instance SubseqAt rest sub right => SubseqAt ('R ': rest) sub (left ':+: right) where
  weakenAt     = weakenRight . weakenAt @rest
  strengthenAt = either (const Nothing) (strengthenAt @rest) . decompose

  type ReplacedAt ('R ': rest) sub sub' (left ':+: right) = left ':+: ReplacedAt rest sub sub' right
  replaceAt = replaceRight . replaceAt @rest
  splitAt p = first weakenRight . splitAt @rest p <=< splitRight


class Subseq sub super => ProperSubseq sub super where
  type family super \\ sub :: Seq (Type -> Type)
  delete :: Union super a -> Either (Union (super \\ sub) a) (Union sub a)

instance (PathTo sub super ~ path, ProperSubseqAt path sub super) => ProperSubseq sub super where
  type super \\ sub = DeletedAt (PathTo sub super) sub super
  delete = deleteAt @path


class SubseqAt path sub super => ProperSubseqAt path sub super where
  type family DeletedAt path sub super :: Seq (Type -> Type)
  deleteAt :: Union super a -> Either (Union (DeletedAt path sub super) a) (Union sub a)

instance ProperSubseqAt ('L ': '[]) left (left :+: right) where
  type DeletedAt ('L ': '[]) left (left :+: right) = right
  deleteAt = either Right Left . decompose

instance ProperSubseqAt ('R ': '[]) right (left :+: right) where
  type DeletedAt ('R ': '[]) right (left :+: right) = left
  deleteAt = decompose

instance ProperSubseqAt (next ': rest) sub left => ProperSubseqAt ('L ': next ': rest) sub (left :+: right) where
  type DeletedAt ('L ': next ': rest) sub (left :+: right) = DeletedAt (next ': rest) sub left :+: right
  deleteAt = either (first weakenLeft . deleteAt @(next ': rest)) (Left . weakenRight) . decompose

instance ProperSubseqAt (next ': rest) sub right => ProperSubseqAt ('R ': next ': rest) sub (left :+: right) where
  type DeletedAt ('R ': next ': rest) sub (left :+: right) = left :+: DeletedAt (next ': rest) sub right
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
