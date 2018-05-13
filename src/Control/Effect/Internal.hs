{-# LANGUAGE DataKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}
module Control.Effect.Internal where

import Control.Applicative
import Control.Category
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Effect.Union
import Data.Functor.Classes (Show1(..), showsBinaryWith, showsUnaryWith)
import Data.TASequence.BinaryTree
import Prelude hiding (id, (.))

data Effect effects result
  = Pure result
  | forall incremental . Effect (Union effects incremental) (Queue effects incremental result)

type Queue effects = BinaryTree (Arrow effects)


send :: Member effect effects => effect return -> Effect effects return
send effect = Effect (inject effect) id


run :: Effect 'Z a -> a
run (Pure a)     = a
run (Effect _ _) = error "impossible"

runM :: Monad m => Effect ('S m) a -> m a
runM (Pure a)     = pure a
runM (Effect u q) = strengthenSingleton u >>= runM . dequeue q


dequeue :: Queue effects a b -> a -> Effect effects b
dequeue q' x = case tviewl q' of
  TAEmptyL -> pure x
  k :< t   -> case runArrow k x of
    Pure y -> dequeue t y
    Effect u q -> Effect u (t <<< q)


newtype Arrow effects a b = Arrow { runArrow :: a -> Effect effects b }
  deriving (Functor)

instance Category (Arrow effects) where
  id = Arrow pure
  Arrow f . Arrow g = Arrow (f <=< g)

instance Applicative (Arrow effects a) where
  pure = Arrow . const . pure
  Arrow f <*> Arrow a = Arrow ((<*>) <$> f <*> a)

instance Member Nondeterminism effects => Alternative (Arrow effects a) where
  empty = Arrow (const empty)
  Arrow l <|> Arrow r = Arrow ((<|>) <$> l <*> r)

instance Monad (Arrow effects a) where
  return = pure
  Arrow m >>= f = Arrow (\ e -> do
    a <- m e
    runArrow (f a) e)

instance Show (Arrow effects a b) where
  showsPrec d (Arrow _) = showParen (d > 10) $ showString "Arrow _"


instance Functor (Effect effects) where
  fmap f (Pure a)     = Pure (f a)
  fmap f (Effect u q) = Effect u (q |> Arrow (Pure . f))

instance Applicative (Effect effects) where
  pure = Pure
  Pure f     <*> Pure a     = Pure (f a)
  Pure f     <*> Effect u q = Effect u (q |> Arrow (Pure . f))
  Effect u q <*> m          = Effect u (q |> Arrow (<$> m))

instance Member Nondeterminism effects => Alternative (Effect effects) where
  empty = send Zero
  l <|> r = send Plus >>= bool l r

instance Monad (Effect effects) where
  return = pure
  Pure a     >>= f = f a
  Effect u q >>= f = Effect u (q |> Arrow f)

instance (Show result, Show1 (Union effects)) => Show (Effect effects result) where
  showsPrec d eff = case eff of
    Pure a     -> showsUnaryWith showsPrec "Pure" d a
    Effect u q -> showsBinaryWith (liftShowsPrec hide hideList) showsPrec "Effect" d u q
    where hide = const (const (showString ""))
          hideList = const (showString "")


data Nondeterminism result where
  Zero :: Nondeterminism result
  Plus :: Nondeterminism Bool

deriving instance Show (Nondeterminism result)

instance Show1 Nondeterminism where
  liftShowsPrec _ _ = showsPrec
