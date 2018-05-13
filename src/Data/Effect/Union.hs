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
, Replace(..)
) where

import Control.Monad ((<=<))
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

instance (PathTo sub super ~ path, SubseqAt path sub super) => Subseq sub super where
  weaken = weakenAt @path
  strengthen = strengthenAt @path


class SubseqAt (path :: [Side]) sub super where
  weakenAt     :: Union sub   a ->        Union super a
  strengthenAt :: Union super a -> Maybe (Union sub   a)

instance SubseqAt '[] sub sub where
  weakenAt     = id
  strengthenAt = Just

instance SubseqAt rest sub left => SubseqAt ('L ': rest) sub (left ':+: right) where
  weakenAt     = weakenLeft . weakenAt @rest
  strengthenAt = either (strengthenAt @rest) (const Nothing) . decompose

instance SubseqAt rest sub right => SubseqAt ('R ': rest) sub (left ':+: right) where
  weakenAt     = weakenRight . weakenAt @rest
  strengthenAt = either (const Nothing) (strengthenAt @rest) . decompose


class (Subseq sub super, Subseq sub' super') => Replace sub sub' super super' | sub sub' super -> super', sub super super' -> sub', sub sub' super' -> super, sub' super super' -> sub where
  replace :: (Union sub a -> Union sub' a) -> Union super a -> Union super' a
  split   :: Union super a -> Either (Union super' a) (Union sub a)

instance (PathTo sub super ~ path, PathTo sub' super' ~ path, SubseqAt path sub super, SubseqAt path sub' super', ReplaceAt path sub sub' super super') => Replace sub sub' super super' where
  replace = replaceAt @path
  split   = splitAt @path


class ReplaceAt (path :: [Side]) sub sub' super super' | sub sub' super -> super', sub super super' -> sub', sub sub' super' -> super, sub' super super' -> sub where
  replaceAt :: (Union sub a -> Union sub' a) -> Union super a -> Union super' a
  splitAt   :: Union super a -> Either (Union super' a) (Union sub a)

instance ReplaceAt '[] sub sub' sub sub' where
  replaceAt = ($)
  splitAt   = Right

instance ReplaceAt rest sub sub' left left' => ReplaceAt ('L ': rest) sub sub' (left ':+: right) (left' ':+: right) where
  replaceAt = replaceLeft . replaceAt @rest
  splitAt   = either (Left . weakenLeft) Right . splitAt @rest @sub @sub' <=< splitLeft

instance ReplaceAt rest sub sub' right right' => ReplaceAt ('R ': rest) sub sub' (left ':+: right) (left ':+: right') where
  replaceAt = replaceRight . replaceAt @rest
  splitAt   = either (Left . weakenRight) Right . splitAt @rest @sub @sub' <=< splitRight


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
