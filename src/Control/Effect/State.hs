{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, StandaloneDeriving #-}
module Control.Effect.State where

import Control.Effect
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


runState :: state -> Effect ('S (State state)) a -> (a, state)
runState state = runSingletonState state (flip (,)) (\ state eff yield -> case eff of
  Get -> yield state state
  Put state' -> yield state' ())


deriving instance Show state => Show (State state result)

instance Show state => Show1 (State state) where
  liftShowsPrec _ _ = showsPrec
