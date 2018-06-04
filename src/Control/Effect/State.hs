{-# LANGUAGE FlexibleContexts, GADTs, PolyKinds, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, ViewPatterns #-}
module Control.Effect.State where

import Control.Effect
import Control.Effect.Internal
-- import Control.Effect.Reader
-- import Control.Effect.Writer
import Data.Effect.Union
import Data.Functor.Classes (Show1(..))

data State state m result where
  Get ::          State state m state
  Put :: state -> State state m ()


get :: Member (State state) effects =>          Eff effects scopes state
get = send Get

put :: Member (State state) effects => state -> Eff effects scopes ()
put = send . Put

modify :: Member (State state) effects => (state -> state) -> Eff effects scopes ()
modify f = get >>= put . f

modify' :: Member (State state) effects => (state -> state) -> Eff effects scopes ()
modify' f = do
  state <- get
  put $! f state


runState :: ((effects \\ S (State state)) rest, Scope (Union scopes)) => state -> Eff effects scopes a -> Eff rest scopes (state, a)
runState s (Pure a) = pure (s, a)
runState s (Eff u q) = case strengthenSingleton <$> delete u of
  Left u' -> Eff u' (unit (Arrow (yield s)))
  Right Get -> yield s s
  Right (Put s') -> yield s' ()
  where yield s = runState s . dequeue q
runState s (Scope u q) = Scope (handle (s, ()) (uncurry runState) u) (unit (Arrow (runState s . dequeue q . snd)))


-- runState :: (effects \\ S (State state)) rest => state -> Eff effects scopes a -> Eff rest scopes (a, state)
-- runState state = relayStatefulEffect state (\ state' a -> pure (a, state')) (\ state' eff yield -> case eff of
--   Get -> yield state' state'
--   Put state'' -> yield state'' ())
--
-- reinterpretState :: forall state effects effects' a proxy . ((S (State state) >-> (S (Reader state) :+: S (Writer state))) effects effects', Member (Reader state) effects', Member (Writer state) effects') => proxy state -> Eff effects scopes a -> Eff effects scopes' a
-- reinterpretState _ = reinterpretEffect @(State state) pure (\ eff yield -> case eff of
--   Get -> ask >>= yield
--   Put s -> tell s >>= yield)
--
-- runStateRW :: (effects \\ (S (Reader state) :+: S (Writer state))) rest => state -> Eff effects scopes a -> Eff rest scopes (a, state)
-- runStateRW state = relayStatefulEffects state (\ state' a -> pure (a, state')) (\ state' effs yield -> case decompose effs of
--   Left  (strengthenSingleton -> Ask)         -> yield state' state'
--   Right (strengthenSingleton -> Tell state'') -> yield state'' ())


deriving instance Show state => Show (State state m result)

instance Show state => Show1 (State state m) where
  liftShowsPrec _ _ = showsPrec
