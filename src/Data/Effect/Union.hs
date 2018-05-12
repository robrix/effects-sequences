{-# LANGUAGE DataKinds, ExistentialQuantification, KindSignatures #-}
module Data.Effect.Union where

import Data.Effect.BinaryTree

data Union (members :: BinaryTree (* -> *)) a = forall member . Union {-# UNPACK #-} !Int (member a)
