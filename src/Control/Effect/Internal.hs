{-# LANGUAGE DataKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Control.Effect.Internal where

import Control.Applicative
import qualified Control.Arrow as A
import Control.Category
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Effect.Union
import Data.Functor.Classes (Show1(..), showsBinaryWith, showsUnaryWith)
import qualified Data.TASequence.BinaryTree as TA
import Prelude hiding (id, (.))

data Effect effects result
  = Pure result
  | forall incremental . Effect (Union effects incremental) (Queue effects incremental result)


send :: Member effect effects => effect return -> Effect effects return
send effect = Effect (inject effect) id


run :: Effect 'Z a -> a
run (Pure a)     = a
run (Effect _ _) = error "impossible"

runM :: Monad m => Effect ('S m) a -> m a
runM (Pure a)     = pure a
runM (Effect u q) = strengthenSingleton u >>= runM . dequeue q


runSingleton :: (a -> b) -> (forall result . eff result -> (result -> b) -> b) -> Effect ('S eff) a -> b
runSingleton pure' bind = loop
  where loop (Pure a)     = pure' a
        loop (Effect u q) = bind (strengthenSingleton u) (loop . dequeue q)


interpose :: Member effect effects => (a -> Effect effects b) -> (forall result . effect result -> (result -> Effect effects b) -> Effect effects b) -> Effect effects a -> Effect effects b
interpose pure' bind = loop
  where loop (Pure a)           = pure' a
        loop (Effect u q)
          | Just x <- project u = bind x k
          | otherwise           = Effect u (unit (Arrow k))
          where k = loop . dequeue q


newtype Queue effects a b = Queue (TA.BinaryTree (Arrow effects) a b)
  deriving (Show)

instance Category (Queue effects) where
  id = Queue id
  Queue TA.Empty . q              = q
  q              . Queue TA.Empty = q
  Queue q1       . Queue q2       = Queue (q1 <<< q2)

unit :: Arrow effects a b -> Queue effects a b
unit = Queue . TA.tsingleton

(|>) :: Queue effects a b -> Arrow effects b c -> Queue effects a c
Queue TA.Empty |> a = unit a
Queue q        |> a = Queue (q TA.|> a)

dequeue :: Queue effects a b -> a -> Effect effects b
dequeue (Queue q) x = case TA.tviewl q of
  TA.TAEmptyL -> pure x
  k TA.:< t   -> case runArrow k x of
    Pure y     -> dequeue (Queue t) y
    Effect u q -> Effect u (q >>> Queue t)


newtype Arrow effects a b = Arrow { runArrow :: a -> Effect effects b }
  deriving (Functor)

instance Category (Arrow effects) where
  id = Arrow pure
  Arrow f . Arrow g = Arrow (f <=< g)

instance A.Arrow (Arrow effects) where
  arr = Arrow . (pure .)
  Arrow f *** Arrow g = Arrow (\ (a, b) -> (,) <$> f a <*> g b)

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
  fmap f (Effect u q) = Effect u (q |> A.arr f)

instance Applicative (Effect effects) where
  pure = Pure
  Pure f     <*> Pure a     = Pure (f a)
  Pure f     <*> Effect u q = Effect u (q |> A.arr f)
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
