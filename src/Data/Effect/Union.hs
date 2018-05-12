{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts, KindSignatures, ScopedTypeVariables, TypeApplications, TypeInType, TypeOperators #-}
module Data.Effect.Union
( Set
, Union
, weakenSingleton
, strengthenSingleton
, weakenLeft
, weakenRight
) where

import Data.Effect.BinaryTree
import Data.Kind (Type)
import GHC.TypeLits
import Unsafe.Coerce

type Set = BinaryTree

data Union (members :: Set (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)


weakenSingleton :: member a -> Union ('L member) a
weakenSingleton = Union 0

strengthenSingleton :: Union ('L member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union n t

weakenRight :: forall left right a . KnownNat (Size left) => Union right a -> Union (left ':+: right) a
weakenRight (Union n t) = Union (size @left + n) t
