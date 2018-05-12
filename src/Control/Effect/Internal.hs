{-# LANGUAGE DeriveFunctor, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Control.Effect.Internal where

import Control.Category
import Control.Monad ((<=<))
import Data.Effect.Union
import Data.TASequence.BinaryTree
import Prelude hiding (id, (.))

data Effect effects result
  = Pure result
  | forall incremental . Effect (Union effects incremental) (Queue effects incremental result)

type Queue effects = BinaryTree (Arrow effects)

newtype Arrow effects a b = Arrow { runArrow :: a -> Effect effects b }
  deriving (Functor)

instance Category (Arrow effects) where
  id = Arrow pure
  Arrow f . Arrow g = Arrow (f <=< g)

instance Applicative (Arrow effects a) where
  pure = Arrow . const . pure
  Arrow f <*> Arrow a = Arrow ((<*>) <$> f <*> a)


instance Functor (Effect effects) where
  fmap f (Pure a)     = Pure (f a)
  fmap f (Effect u q) = Effect u (q |> Arrow (Pure . f))

instance Applicative (Effect effects) where
  pure = Pure
  Pure f     <*> Pure a     = Pure (f a)
  Pure f     <*> Effect u q = Effect u (q |> Arrow (Pure . f))
  Effect u q <*> m          = Effect u (q |> Arrow (<$> m))

instance Monad (Effect effects) where
  return = pure
  Pure a     >>= f = f a
  Effect u q >>= f = Effect u (q |> Arrow f)
