{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, TypeSynonymInstances, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, Scope(..)
-- * Constructing effects
, send
, scope
-- * Handlers
, run
, runM
-- , interpretEffects
-- , relayEffects
-- , relayStatefulEffects
-- , reinterpretEffects
-- , interpretEffect
-- , relayEffect
-- , relayStatefulEffect
-- , reinterpretEffect
-- , interpose
-- , interposeState
-- , interposeSplit
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
import Data.Effect.Union
import Data.Functor.Identity
import qualified Data.TASequence.BinaryTree as TA
import Prelude hiding (id, (.))

data Eff effects scopes result
  = Pure result
  | forall incremental . Eff   (Union effects Identity incremental) (Queue effects scopes incremental result)
  | Scope (Union scopes (Eff effects scopes) result)
  -- |                      Scope (g            (Prog f g            (Prog f g a)))

send :: Member effect effects => effect Identity return -> Eff effects scopes return
send effect = Eff (inject effect) id

scope :: Member scope scopes => scope (Eff effects scopes) return -> Eff effects scopes return
scope effect = Scope (inject effect)

type f ~> g = forall a . f a -> g a

class Scope scope where
  hmap :: (a ~> b) -> (scope a ~> scope b)
  scopeMap :: Monad m => (m a -> m b) -> (scope m a -> scope m b)
  handleState :: (Monad m, Monad n, Functor c) => c () -> (forall x . c (m x) -> n (c x)) -> (scope m a -> scope n (c a))

  handle :: (Monad m, Monad n) => (forall x . m x -> n x) -> (scope m a -> scope n a)
  handle f s = fmap runIdentity `scopeMap` handleState (Identity ()) (\ (Identity m) -> Identity <$> f m) s

instance Scope member => Scope (Union (S member)) where
  scopeMap f = weakenSingleton . scopeMap f . strengthenSingleton

  handleState c hdl = weakenSingleton . handleState c hdl . strengthenSingleton

instance (Scope (Union left), Scope (Union right)) => Scope (Union (left :+: right)) where
  scopeMap f = either (weakenLeft . scopeMap f) (weakenRight . scopeMap f) . decompose

  handleState c hdl = either (weakenLeft . handleState c hdl) (weakenRight . handleState c hdl) . decompose


run :: Eff Empty Empty a -> a
run (Pure a) = a
run _        = error "impossible: Eff or Scope with no effects or scopes"

runM :: Monad m => Eff (S (Lift m)) Empty a -> m a
runM (Pure a)  = pure a
runM (Eff u q) = unLift (strengthenSingleton u) >>= runM . dequeue q . runIdentity
runM _         = error "impossible: Scope with no scopes"


-- interpretEffects :: forall superEffect subEffect superEffect' superScope subScope superScope' a
--                  .  ( (superEffect \\ subEffect) superEffect'
--                     , (superScope \\ subScope) superScope'
--                     , Scope (Union superScope')
--                     , Scope (Union subScope)
--                     )
--                  => (forall result . Union subEffect Identity result -> Eff superEffect' superScope' result)
--                  -> (forall result . Union subScope (Eff superEffect' superScope') result -> Eff superEffect' superScope' result)
--                  -> Eff superEffect superScope a
--                  -> Eff superEffect' superScope' a
-- interpretEffects handleEffect handleScope = loop
--   where loop :: forall a. Eff superEffect superScope a -> Eff superEffect' superScope' a
--         loop (Pure a)  = pure a
--         loop (Eff u q) = case delete u of
--           Left  u' -> Eff u' (unit (Arrow (loop . dequeue q)))
--           Right u' -> handleEffect u' >>= loop . dequeue q
--         loop (Scope u) = case delete u of
--           Left  u' -> Scope (hmap loop u')
--           Right u' -> handleScope (hmap loop u')

-- relayEffects :: forall superEffect subEffect superEffect' superScope subScope superScope' a c
--                  .  ( (superEffect \\ subEffect) superEffect'
--                     , (superScope \\ subScope) superScope'
--                     , Scope (Union superScope')
--                     , Scope (Union subScope)
--                     , Applicative c
--                     )
--                  => c ()
--                  -> (forall x . x -> Eff superEffect' superScope' (c x))
--                  -> (forall x result . Union subEffect Identity result -> (result -> Eff superEffect' superScope' (c x)) -> Eff superEffect' superScope' (c x))
--                  -> (forall x result . Union subScope (Eff superEffect' superScope') result -> (result -> Eff superEffect' superScope' (c x)) -> Eff superEffect' superScope' (c x))
--                  -> (forall x . c (Eff superEffect superScope x) -> Eff superEffect' superScope' (c x))
--                  -> Eff superEffect superScope a
--                  -> Eff superEffect' superScope' (c a)
-- relayEffects initial pure' handleEffect handleScope distScope = loop
--   where loop :: Eff superEffect superScope x -> Eff superEffect' superScope' (c x)
--         loop (Pure a)  = pure' a
--         loop (Eff u q) = case delete u of
--           Left  u' -> Eff u' (unit (Arrow yield))
--           Right u' -> handleEffect u' yield
--           where yield = loop . dequeue q
--         loop (Scope u q) = case delete u of
--           Left  u' -> Scope (handleState initial distScope u') (unit (Arrow yield))
--           Right u' -> handleScope (scopeMap loop u') yield
--           where yield = loop . dequeue q

        -- runState :: Syntax sig ⇒ (s, Prog (HState s+sig) a) → Prog sig (s,a)
        -- runState s (Other op) = Op (handleState (s, ()) (uncurry runState) op)

-- runException :: Eff … a -> Eff … (Either err a)


-- relayStatefulEffects :: ( (super \\ sub) super'
--                         , Scope (Union super')
--                         , Scope (Union sub)
--                         )
--                      => state
--                      -> (state -> a -> Eff super' a')
--                      -> (forall result . state -> Union sub (Eff super') result -> (state -> result -> Eff super' a') -> Eff super' a')
--                      -> Eff super a
--                      -> Eff super' a'
-- relayStatefulEffects initial pure' bind = loop initial
--   where loop state (Pure a)  = pure' state a
--         loop state (Eff u q) = case delete u of
--           Left  u' -> Eff u' (unit (Arrow (loop state . dequeue q)))
--           Right u' -> bind state u' (\ state' -> loop state' . dequeue q)
--
-- reinterpretEffects :: ( (sub >-> sub') super super'
--                       , Scope (Union super')
--                       , Scope (Union sub)
--                       )
--                    => (a -> Eff super' a')
--                    -> (forall result . Union sub (Eff super') result -> (result -> Eff super' a') -> Eff super' a')
--                    -> Eff super a
--                    -> Eff super' a'
-- reinterpretEffects pure' bind = loop
--   where loop (Pure a)  = pure' a
--         loop (Eff u q) = case split u of
--           Left  u' -> Eff u' (unit (Arrow (loop . dequeue q)))
--           Right u' -> bind u' (loop . dequeue q)
--
--
-- interpretEffect :: ( (super \\ S effect) super'
--                    , Scope (Union super')
--                    , Scope effect
--                    )
--                 => (forall result . effect (Eff super') result -> Eff super' result)
--                 -> Eff super a
--                 -> Eff super' a
-- interpretEffect handler = interpretEffects (handler . strengthenSingleton)
--
-- relayEffect :: ( (super \\ S effect) super'
--                , Scope (Union super')
--                , Scope effect
--                )
--             => (a -> Eff super' a')
--             -> (forall result . effect (Eff super') result -> (result -> Eff super' a') -> Eff super' a')
--             -> Eff super a
--             -> Eff super' a'
-- relayEffect pure' bind = relayEffects pure' (bind . strengthenSingleton)
--
-- relayStatefulEffect :: ( (super \\ S effect) super'
--                        , Scope (Union super')
--                        , Scope effect
--                        )
--                     => state
--                     -> (state -> a -> Eff super' a')
--                     -> (forall result . state -> effect (Eff super') result -> (state -> result -> Eff super' a') -> Eff super' a')
--                     -> Eff super a
--                     -> Eff super' a'
-- relayStatefulEffect state pure' bind = relayStatefulEffects state pure' (\ state' -> bind state' . strengthenSingleton)
--
-- reinterpretEffect :: ( (S effect >-> sub') super super'
--                      , Scope (Union super')
--                      , Scope effect
--                      )
--                   => (a -> Eff super' a')
--                   -> (forall result . effect (Eff super') result -> (result -> Eff super' a') -> Eff super' a')
--                   -> Eff super a
--                   -> Eff super' a'
-- reinterpretEffect pure' bind = reinterpretEffects pure' (bind . strengthenSingleton)


-- interpose :: Member effect effects
--           => (a -> Eff effects b)
--           -> (forall result . effect (Eff effects) result -> (result -> Eff effects b) -> Eff effects b)
--           -> Eff effects a
--           -> Eff effects b
-- interpose pure' bind = loop
--   where loop (Pure a)           = pure' a
--         loop (Eff u q)
--           | Just x <- project u = bind x k
--           | otherwise           = Eff u (unit (Arrow k))
--           where k = loop . dequeue q
--
-- interposeState :: Member effect effects
--                => state
--                -> (state -> a -> Eff effects b)
--                -> (forall result . state -> effect (Eff effects) result -> (state -> result -> Eff effects b) -> Eff effects b)
--                -> Eff effects a
--                -> Eff effects b
-- interposeState state pure' bind = interposeSplit state pure' (\ state' eff yield loop -> bind state' eff (\ state'' -> loop state'' . yield))
--
-- interposeSplit :: Member effect effects
--                => state
--                -> (state -> a -> Eff effects b)
--                -> (forall result . state -> effect (Eff effects) result -> (result -> Eff effects a) -> (state -> Eff effects a -> Eff effects b) -> Eff effects b)
--                -> Eff effects a
--                -> Eff effects b
-- interposeSplit initial pure' bind = loop initial
--   where loop state (Pure a)  = pure' state a
--         loop state (Eff u q) = case project u of
--           Just effect -> bind state effect (dequeue q) loop
--           _           -> Eff u (unit (Arrow (loop state . dequeue q)))

newtype Queue effects scopes a b = Queue (TA.BinaryTree (Arrow effects scopes) a b)
  deriving (Show)

instance Category (Queue effects scopes) where
  id = Queue id
  Queue TA.Empty . q              = q
  q              . Queue TA.Empty = q
  Queue q1       . Queue q2       = Queue (q1 <<< q2)

unit :: Arrow effects scopes a b -> Queue effects scopes a b
unit = Queue . TA.tsingleton

(|>) :: Queue effects scopes a b -> Arrow effects scopes b c -> Queue effects scopes a c
Queue TA.Empty |> a = unit a
Queue q        |> a = Queue (q TA.|> a)

dequeue :: Queue effects scopes a b -> a -> Eff effects scopes b
dequeue (Queue q) x = case TA.tviewl q of
  TA.TAEmptyL -> pure x
  k TA.:< t -> case runArrow k x of
    Pure y  -> dequeue (Queue t) y
    Eff u q -> Eff u (q >>> Queue t)


newtype Arrow effects scopes a b = Arrow { runArrow :: a -> Eff effects scopes b }
  deriving (Functor)

instance Category (Arrow effects scopes) where
  id = Arrow pure
  Arrow f . Arrow g = Arrow (f <=< g)

instance A.Arrow (Arrow effects scopes) where
  arr = Arrow . (pure .)
  Arrow f *** Arrow g = Arrow (\ (a, b) -> (,) <$> f a <*> g b)

instance Applicative (Arrow effects scopes a) where
  pure = Arrow . const . pure
  Arrow f <*> Arrow a = Arrow ((<*>) <$> f <*> a)

instance Member Nondeterminism effects => Alternative (Arrow effects scopes a) where
  empty = Arrow (const empty)
  Arrow l <|> Arrow r = Arrow ((<|>) <$> l <*> r)

instance Monad (Arrow effects scopes a) where
  return = pure
  Arrow m >>= f = Arrow (\ e -> do
    a <- m e
    runArrow (f a) e)

instance Show (Arrow effects scopes a b) where
  showsPrec d (Arrow _) = showParen (d > 10) $ showString "Arrow _"


instance Functor (Eff effects scopes) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Eff u q) = Eff u (q |> A.arr f)

instance Applicative (Eff effects scopes) where
  pure = Pure
  Pure f  <*> Pure a  = Pure (f a)
  Pure f  <*> Eff u q = Eff u (q |> A.arr f)
  Eff u q <*> m       = Eff u (q |> Arrow (<$> m))

instance Member Nondeterminism effects => Alternative (Eff effects scopes) where
  empty = send Zero
  l <|> r = send Plus >>= \ cond -> if cond then l else r

instance Monad (Eff effects scopes) where
  return = pure
  Pure a  >>= f = f a
  Eff u q >>= f = Eff u (q |> Arrow f)

instance Member Fail effects => MonadFail (Eff effects scopes) where
  fail = send . Fail

instance Member Nondeterminism effects => MonadPlus (Eff effects scopes) where
  mzero = empty
  mplus = (<|>)


newtype Lift effect m a = Lift { unLift :: effect (m a) }
  deriving (Functor)

instance Functor effect => Scope (Lift effect) where
  hmap f (Lift effect) = Lift (fmap f effect)
  scopeMap f (Lift effect) = Lift (f <$> effect)
  handleState c hdl (Lift effect) = Lift (hdl . (<$ c) <$> effect)


data Nondeterminism m result where
  Zero :: Nondeterminism m result
  Plus :: Nondeterminism m Bool

deriving instance Show (Nondeterminism m result)


newtype Fail m result = Fail String
  deriving (Show)

-- instance Scope Fail where
--   hmap _ (Fail s) = Fail s
--
-- instance Scope Fail where
--   emap _ (Fail s) = Fail s
