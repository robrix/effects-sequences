{-# LANGUAGE DataKinds, ExistentialQuantification, KindSignatures, TypeInType #-}
module Data.Effect.Union where

import Data.Effect.BinaryTree
import Data.Kind (Type)
import Unsafe.Coerce

type Set = BinaryTree

data Union (members :: Set (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)


weakenSingleton :: member a -> Union ('L member) a
weakenSingleton = Union 0

strengthenSingleton :: Union ('L member) a -> member a
strengthenSingleton (Union _ member) = unsafeCoerce member
