{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
, Subseq
, weaken
, weakenLeft
, weakenRight
, strengthen
, type (>->)
, replace
, split
, type (\\)
, delete
) where

import Control.Monad ((<=<))
import Data.Bifunctor
import Data.Effect.Sequence
import Data.Functor.Classes (Show1(..))
import Data.Kind (Type)
import Prelude hiding (splitAt)
import Unsafe.Coerce

data Union (members :: Seq ((k -> *) -> k -> Type)) (m :: k -> Type) (a :: k) = forall member . Union {-# UNPACK #-} !Int (member m a)

type Member member members = Subseq (S member) members

inject :: Member member members => member m a -> Union members m a
inject = weaken . weakenSingleton

project :: Member member members => Union members m a -> Maybe (member m a)
project = fmap strengthenSingleton . strengthen


weakenSingleton :: member m a -> Union (S member) m a
weakenSingleton = Union 0

strengthenSingleton :: Union (S member) m a -> member m a
strengthenSingleton (Union _ member) = unsafeCoerce member


decompose :: Union (left ':+: right) m a -> Either (Union left m a) (Union right m a)
decompose (Union n member)
  | even n    = Left  (Union (n `div` 2) member)
  | otherwise = Right (Union (n `div` 2) member)


type Subseq sub super = SubseqAt (PathTo sub super) sub super

weaken :: forall sub super m a . Subseq sub super => Union sub m a -> Union super m a
weaken = weakenAt @(PathTo sub super)

strengthen :: forall sub super m a . Subseq sub super => Union super m a -> Maybe (Union sub m a)
strengthen = strengthenAt @(PathTo sub super)


type (sub >-> sub') super super' = ReplaceAt (PathTo sub super) sub sub' super super'

replace :: forall sub sub' super super' m a . (sub >-> sub') super super' => (Union sub m a -> Union sub' m a) -> Union super m a -> Union super' m a
replace = replaceAt @(PathTo sub super)

split :: forall sub sub' super super' m a . (sub >-> sub') super super' => Union super m a -> Either (Union super' m a) (Union sub m a)
split = splitAt @(PathTo sub super)


type (super \\ sub) difference = DifferenceAt (PathTo sub super) sub super difference

delete :: forall super sub difference m a . (super \\ sub) difference => Union super m a -> Either (Union difference m a) (Union sub m a)
delete = deleteAt @(PathTo sub super)


class SubseqAt (path :: [Side]) sub super | path super -> sub where
  weakenAt     :: Union sub   m a ->        Union super m a
  strengthenAt :: Union super m a -> Maybe (Union sub   m a)

instance SubseqAt '[] sub sub where
  weakenAt     = id
  strengthenAt = Just

instance SubseqAt rest sub left => SubseqAt ('L ': rest) sub (left ':+: right) where
  weakenAt     = weakenLeft . weakenAt @rest
  strengthenAt = either (strengthenAt @rest) (const Nothing) . decompose

instance SubseqAt rest sub right => SubseqAt ('R ': rest) sub (left ':+: right) where
  weakenAt     = weakenRight . weakenAt @rest
  strengthenAt = either (const Nothing) (strengthenAt @rest) . decompose


class (SubseqAt path sub super, SubseqAt path sub' super') => ReplaceAt path sub sub' super super'
  | path super -> sub
  , path super' -> sub'
  , super sub sub' -> super'
  , super sub super' -> sub'
  , super sub' super' -> sub
  , sub sub' super' -> super where
  replaceAt :: (Union sub m a -> Union sub' m a) -> Union super m a -> Union super' m a
  splitAt :: Union super m a -> Either (Union super' m a) (Union sub m a)

instance ReplaceAt '[] sub sub' sub sub' where
  replaceAt = ($)
  splitAt   = Right

instance ReplaceAt rest sub sub' left left' => ReplaceAt ('L ': rest) sub sub' (left :+: right) (left' :+: right) where
  replaceAt = replaceLeft . replaceAt @rest
  splitAt = first weakenLeft . splitAt @rest <=< splitLeft

instance ReplaceAt rest sub sub' right right' => ReplaceAt ('R ': rest) sub sub' (left :+: right) (left :+: right') where
  replaceAt = replaceRight . replaceAt @rest
  splitAt = first weakenRight . splitAt @rest <=< splitRight


class SubseqAt path sub super => DifferenceAt path sub super (difference :: Seq ((Type -> Type) -> Type -> Type)) | path super -> sub difference, super difference -> sub where
  deleteAt :: Union super m a -> Either (Union difference m a) (Union sub m a)

instance Diff sub Empty ~ sub' => DifferenceAt '[] sub' sub Empty where
  deleteAt = Right

instance DifferenceAt ('L ': '[]) left (left :+: right) right where
  deleteAt = either Right Left . decompose

instance DifferenceAt (next ': rest) sub left left' => DifferenceAt ('L ': next ': rest) sub (left :+: right) (left' :+: right) where
  deleteAt = either (first weakenLeft . deleteAt @(next ': rest)) (Left . weakenRight) . decompose

instance DifferenceAt ('R ': '[]) right (left :+: right) left where
  deleteAt = decompose

instance DifferenceAt (next ': rest) sub right right' => DifferenceAt ('R ': next ': rest) sub (left :+: right) (left :+: right') where
  deleteAt = either (Left . weakenLeft) (first weakenRight . deleteAt @(next ': rest)) . decompose


weakenLeft :: Union left m a -> Union (left ':+: right) m a
weakenLeft (Union n t) = Union (2 * n) t

weakenRight :: Union right m a -> Union (left ':+: right) m a
weakenRight (Union n t) = Union (1 + (2 * n)) t


replaceLeft :: (Union left m a -> Union left' m a) -> Union (left ':+: right) m a ->  Union (left' ':+: right) m a
replaceLeft f = either (weakenLeft . f) weakenRight . decompose

replaceRight :: (Union right m a -> Union right' m a) -> Union (left ':+: right) m a ->  Union (left ':+: right') m a
replaceRight f = either weakenLeft (weakenRight . f) . decompose


splitLeft :: Union (left ':+: right) m a -> Either (Union (left' ':+: right) m a) (Union left m a)
splitLeft = either Right (Left . weakenRight) . decompose

splitRight :: Union (left ':+: right) m a -> Either (Union (left ':+: right') m a) (Union right m a)
splitRight = either (Left . weakenLeft) Right . decompose


instance Show (member m a) => Show (Union (S member) m a) where
  showsPrec d = showsPrec d . strengthenSingleton

instance (Show (Union left m a), Show (Union right m a)) => Show (Union (left ':+: right) m a) where
  showsPrec d = either (showsPrec d) (showsPrec d) . decompose

instance Show1 (member m) => Show1 (Union (S member) m) where
  liftShowsPrec sp sl d = liftShowsPrec sp sl d . strengthenSingleton

instance (Show1 (Union left m), Show1 (Union right m)) => Show1 (Union (left ':+: right) m) where
  liftShowsPrec sp sl d = either (liftShowsPrec sp sl d) (liftShowsPrec sp sl d) . decompose

instance Functor (member f) => Functor (Union (S member) f) where
  fmap f = weakenSingleton . fmap f . strengthenSingleton

instance (Functor (Union left f), Functor (Union right f)) => Functor (Union (left :+: right) f) where
  fmap f = either (weakenLeft . fmap f) (weakenRight . fmap f) . decompose
