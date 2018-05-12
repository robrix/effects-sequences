{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}
module Control.Effect.Internal where

import Data.Effect.Union
import Data.TASequence.BinaryTree

data Effect effects result
  = Pure result
  | forall incremental . Effect (Union effects incremental) (Queue effects incremental result)

type Queue effects = BinaryTree (Arrow effects)

newtype Arrow effects a b = Arrow { runArrow :: a -> Effect effects b }
