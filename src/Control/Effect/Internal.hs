{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, RankNTypes, StandaloneDeriving, TypeOperators, TypeSynonymInstances, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, Effect(..)
-- * Constructing effects
, send
-- * Handlers
, run
, runM
, interpretEffects
, relayEffects
, relayStatefulEffects
, reinterpretEffects
, interpretEffect
, relayEffect
, relayStatefulEffect
, reinterpretEffect
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
, Lift(..)
, Nondeterminism(..)
, Fail(..)
) where

import Control.Applicative
import qualified Control.Arrow as A
import Control.Category
import Control.Monad (MonadPlus(..), (<=<))
import Control.Monad.Fail
import Data.Effect.Higher.Functor
import Data.Effect.Union
import Data.Functor.Classes (Show1(..), showsBinaryWith, showsUnaryWith)
import qualified Data.TASequence.BinaryTree as TA
import Prelude hiding (id, (.))

data Eff effects result
  = Pure result
  | forall incremental . Eff (Union effects incremental) (Queue effects incremental result)


send :: Member effect effects => effect return -> Eff effects return
send effect = Eff (inject effect) id


class HFunctor effect => Effect effect where
  emap :: Monad m => (m a -> m b) -> effect m a -> effect m b


run :: Eff Empty a -> a
run (Pure a) = a
run _        = error "impossible: Eff with no effects"

runM :: Monad m => Eff (S m) a -> m a
runM (Pure a)  = pure a
runM (Eff u q) = strengthenSingleton u >>= runM . dequeue q


interpretEffects :: (super \\ sub) super'
                 => (forall result . Union sub result -> Eff super' result)
                 -> Eff super a
                 -> Eff super' a
interpretEffects handler = relayEffects pure ((>>=) . handler)

relayEffects :: (super \\ sub) super'
             => (a -> Eff super' a')
             -> (forall result . Union sub result -> (result -> Eff super' a') -> Eff super' a')
             -> Eff super a
             -> Eff super' a'
relayEffects pure' bind = loop
  where loop (Pure a)  = pure' a
        loop (Eff u q) = case delete u of
          Left  u' -> Eff u' (unit (Arrow (loop . dequeue q)))
          Right u' -> bind u' (loop . dequeue q)

relayStatefulEffects :: (super \\ sub) super'
                     => state
                     -> (state -> a -> Eff super' a')
                     -> (forall result . state -> Union sub result -> (state -> result -> Eff super' a') -> Eff super' a')
                     -> Eff super a
                     -> Eff super' a'
relayStatefulEffects initial pure' bind = loop initial
  where loop state (Pure a)  = pure' state a
        loop state (Eff u q) = case delete u of
          Left  u' -> Eff u' (unit (Arrow (loop state . dequeue q)))
          Right u' -> bind state u' (\ state' -> loop state' . dequeue q)

reinterpretEffects :: (sub >-> sub') super super'
                   => (a -> Eff super' a')
                   -> (forall result . Union sub result -> (result -> Eff super' a') -> Eff super' a')
                   -> Eff super a
                   -> Eff super' a'
reinterpretEffects pure' bind = loop
  where loop (Pure a)  = pure' a
        loop (Eff u q) = case split u of
          Left  u' -> Eff u' (unit (Arrow (loop . dequeue q)))
          Right u' -> bind u' (loop . dequeue q)


interpretEffect :: (super \\ S effect) super'
                => (forall result . effect result -> Eff super' result)
                -> Eff super a
                -> Eff super' a
interpretEffect handler = interpretEffects (handler . strengthenSingleton)

relayEffect :: (super \\ S effect) super'
            => (a -> Eff super' a')
            -> (forall result . effect result -> (result -> Eff super' a') -> Eff super' a')
            -> Eff super a
            -> Eff super' a'
relayEffect pure' bind = relayEffects pure' (bind . strengthenSingleton)

relayStatefulEffect :: (super \\ S effect) super'
                    => state
                    -> (state -> a -> Eff super' a')
                    -> (forall result . state -> effect result -> (state -> result -> Eff super' a') -> Eff super' a')
                    -> Eff super a
                    -> Eff super' a'
relayStatefulEffect state pure' bind = relayStatefulEffects state pure' (\ state' -> bind state' . strengthenSingleton)

reinterpretEffect :: (S effect >-> sub') super super'
                  => (a -> Eff super' a')
                  -> (forall result . effect result -> (result -> Eff super' a') -> Eff super' a')
                  -> Eff super a
                  -> Eff super' a'
reinterpretEffect pure' bind = reinterpretEffects pure' (bind . strengthenSingleton)


interpose :: Member effect effects
          => (a -> Eff effects b)
          -> (forall result . effect result -> (result -> Eff effects b) -> Eff effects b)
          -> Eff effects a
          -> Eff effects b
interpose pure' bind = loop
  where loop (Pure a)           = pure' a
        loop (Eff u q)
          | Just x <- project u = bind x k
          | otherwise           = Eff u (unit (Arrow k))
          where k = loop . dequeue q

interposeState :: Member effect effects
               => state
               -> (state -> a -> Eff effects b)
               -> (forall result . state -> effect result -> (state -> result -> Eff effects b) -> Eff effects b)
               -> Eff effects a
               -> Eff effects b
interposeState state pure' bind = interposeSplit state pure' (\ state' eff yield loop -> bind state' eff (\ state'' -> loop state'' . yield))

interposeSplit :: Member effect effects
               => state
               -> (state -> a -> Eff effects b)
               -> (forall result . state -> effect result -> (result -> Eff effects a) -> (state -> Eff effects a -> Eff effects b) -> Eff effects b)
               -> Eff effects a
               -> Eff effects b
interposeSplit initial pure' bind = loop initial
  where loop state (Pure a)  = pure' state a
        loop state (Eff u q) = case project u of
          Just effect -> bind state effect (dequeue q) loop
          _           -> Eff u (unit (Arrow (loop state . dequeue q)))

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

dequeue :: Queue effects a b -> a -> Eff effects b
dequeue (Queue q) x = case TA.tviewl q of
  TA.TAEmptyL -> pure x
  k TA.:< t -> case runArrow k x of
    Pure y  -> dequeue (Queue t) y
    Eff u q -> Eff u (q >>> Queue t)


newtype Arrow effects a b = Arrow { runArrow :: a -> Eff effects b }
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


instance Functor (Eff effects) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Eff u q) = Eff u (q |> A.arr f)

instance Applicative (Eff effects) where
  pure = Pure
  Pure f  <*> Pure a  = Pure (f a)
  Pure f  <*> Eff u q = Eff u (q |> A.arr f)
  Eff u q <*> m       = Eff u (q |> Arrow (<$> m))

instance Member Nondeterminism effects => Alternative (Eff effects) where
  empty = send Zero
  l <|> r = send Plus >>= \ cond -> if cond then l else r

instance Monad (Eff effects) where
  return = pure
  Pure a  >>= f = f a
  Eff u q >>= f = Eff u (q |> Arrow f)

instance Member Fail effects => MonadFail (Eff effects) where
  fail = send . Fail

instance Member Nondeterminism effects => MonadPlus (Eff effects) where
  mzero = empty
  mplus = (<|>)

instance (Show result, Show1 (Union effects)) => Show (Eff effects result) where
  showsPrec d eff = case eff of
    Pure a     -> showsUnaryWith showsPrec "Pure" d a
    Eff u q -> showsBinaryWith (liftShowsPrec hide hideList) showsPrec "Eff" d u q
    where hide = const (const (showString ""))
          hideList = const (showString "")


newtype Lift effect m a = Lift { unLift :: effect a }
  deriving (Functor)

instance HFunctor (Lift effect) where
  hfmap _ (Lift effect) = Lift effect


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
