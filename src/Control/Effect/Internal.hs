{-# LANGUAGE DataKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Internal
( Effect(..)
-- * Constructing effects
, send
-- * Handlers
, runM
, handleEffect
, Handle(..)
, runSingletonState
, interpose
, interposeState
, interposeSplit
-- * Queues
, Queue(..)
, unit
, (|>)
, dequeue
-- * Arrows
, Arrow(..)
-- * Effects
, Nondeterminism(..)
, Fail(..)
) where

import Control.Applicative
import qualified Control.Arrow as A
import Control.Category
import Control.Monad (MonadPlus(..), (<=<))
import Control.Monad.Fail
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


runM :: Monad m => Effect ('S m) a -> m a
runM = handleEffect pure (>>=)


handleEffect :: (a -> b) -> (forall result . effect result -> (result -> b) -> b) -> Effect ('S effect) a -> b
handleEffect pure' bind = handleEffects pure' (bind . strengthenSingleton)

class Handle effects where
  handleEffects :: (a -> b) -> (forall result . Union effects result -> (result -> b) -> b) -> Effect effects a -> b

instance Handle ('S effect) where
  handleEffects pure' bind = loop
    where loop (Pure a)     = pure' a
          loop (Effect u q) = bind u (loop . dequeue q)

runLeft :: KnownNat (Size left) => (a -> Effect right b) -> (forall result . Union left result -> (result -> Effect right b) -> Effect right b) -> Effect (left ':+: right) a -> Effect right b
runLeft pure' bind = loop
  where loop (Pure a) = pure' a
        loop (Effect u q) = case decompose u of
          Left  u' -> bind   u'              (loop . dequeue q)
          Right u' -> Effect u' (unit (Arrow (loop . dequeue q)))

runSingletonState :: state -> (state -> a -> b) -> (forall result . state -> eff result -> (state -> result -> b) -> b) -> Effect ('S eff) a -> b
runSingletonState state pure' bind = loop state
  where loop state (Pure a)     = pure' state a
        loop state (Effect u q) = bind state (strengthenSingleton u) (\ state' -> loop state' . dequeue q)


interpose :: Member effect effects => (a -> Effect effects b) -> (forall result . effect result -> (result -> Effect effects b) -> Effect effects b) -> Effect effects a -> Effect effects b
interpose pure' bind = loop
  where loop (Pure a)           = pure' a
        loop (Effect u q)
          | Just x <- project u = bind x k
          | otherwise           = Effect u (unit (Arrow k))
          where k = loop . dequeue q

interposeState :: Member effect effects
               => state
               -> (state -> a -> Effect effects b)
               -> (forall result . state -> effect result -> (state -> result -> Effect effects b) -> Effect effects b)
               -> Effect effects a
               -> Effect effects b
interposeState state pure' bind = interposeSplit state pure' (\ state' eff yield loop -> bind state' eff (\ state'' -> loop state'' . yield))

interposeSplit :: Member effect effects
               => state
               -> (state -> a -> Effect effects b)
               -> (forall result . state -> effect result -> (result -> Effect effects a) -> (state -> Effect effects a -> Effect effects b) -> Effect effects b)
               -> Effect effects a
               -> Effect effects b
interposeSplit initial pure' bind = loop initial
  where loop state (Pure a)     = pure' state a
        loop state (Effect u q) = case project u of
          Just effect -> bind state effect (dequeue q) loop
          _           -> Effect u (unit (Arrow (loop state . dequeue q)))

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

instance Member Fail effects => MonadFail (Effect effects) where
  fail = send . Fail

instance Member Nondeterminism effects => MonadPlus (Effect effects) where
  mzero = empty
  mplus = (<|>)

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


newtype Fail result = Fail String
  deriving (Show)

instance Show1 Fail where
  liftShowsPrec _ _ = showsPrec
