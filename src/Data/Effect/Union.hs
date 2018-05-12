{-# LANGUAGE DataKinds, ExistentialQuantification, KindSignatures, TypeInType, TypeOperators #-}
module Data.Effect.Union
( Set
, Union
, weakenSingleton
, strengthenSingleton
, weakenLeft
) where

import Data.Effect.BinaryTree
import Data.Kind (Type)
import Unsafe.Coerce

type Set = BinaryTree

data Union (members :: Set (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)


weakenSingleton :: member a -> Union ('L member) a
weakenSingleton = Union 0

strengthenSingleton :: Union ('L member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member


weakenLeft :: Union left a -> Union (left ':+: right) a
weakenLeft (Union n t) = Union n t
