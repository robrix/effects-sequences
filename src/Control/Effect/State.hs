{-# LANGUAGE FlexibleContexts, GADTs, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, ViewPatterns #-}
module Control.Effect.State where

import Control.Effect
import Control.Effect.Reader
import Control.Effect.Writer
import Data.Effect.Union (strengthenSingleton)
import Data.Functor.Classes (Show1(..))

data State state result where
  Get ::          State state state
  Put :: state -> State state ()


get :: Member (State state) effects =>          Effect effects state
get = send Get

put :: Member (State state) effects => state -> Effect effects ()
put = send . Put

modify :: Member (State state) effects => (state -> state) -> Effect effects ()
modify f = get >>= put . f

modify' :: Member (State state) effects => (state -> state) -> Effect effects ()
modify' f = do
  state <- get
  put $! f state


runState :: (effects \\ S (State state)) rest => state -> Effect effects a -> Effect rest (a, state)
runState state = interpretStatefulEffect state (\ state' a -> pure (a, state')) (\ state' eff yield -> case eff of
  Get -> yield state' state'
  Put state'' -> yield state'' ())

reinterpretState :: forall state effects effects' a proxy . ((S (State state) >-> (S (Reader state) :+: S (Writer state))) effects effects', Member (Reader state) effects', Member (Writer state) effects') => proxy state -> Effect effects a -> Effect effects' a
reinterpretState _ = reinterpretEffect @(State state) pure (\ eff yield -> case eff of
  Get -> ask >>= yield
  Put s -> tell s >>= yield)

runStateRW :: (effects \\ (S (Reader state) :+: S (Writer state))) rest => state -> Effect effects a -> Effect rest (a, state)
runStateRW state = interpretStatefulEffects state (\ state' a -> pure (a, state')) (\ state' effs yield -> case decompose effs of
  Left  (strengthenSingleton -> Ask)         -> yield state' state'
  Right (strengthenSingleton -> Tell state'') -> yield state'' ())


deriving instance Show state => Show (State state result)

instance Show state => Show1 (State state) where
  liftShowsPrec _ _ = showsPrec
