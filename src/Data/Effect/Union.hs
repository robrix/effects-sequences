{-# LANGUAGE DataKinds, ExistentialQuantification, KindSignatures, TypeInType #-}
module Data.Effect.Union where

import Data.Effect.BinaryTree
import Data.Kind (Type)

type Set = BinaryTree

data Union (members :: Set (Type -> Type)) a = forall member . Union {-# UNPACK #-} !Int (member a)
