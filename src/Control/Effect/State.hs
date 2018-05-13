{-# LANGUAGE FlexibleContexts, GADTs #-}
module Control.Effect.State where

import Control.Effect

data State state result where
  Get ::          State state state
  Put :: state -> State state ()


get :: Member (State state) effects =>          Effect effects state
get = send Get

put :: Member (State state) effects => state -> Effect effects ()
put = send . Put

modify :: Member (State state) effects => (state -> state) -> Effect effects ()
modify f = get >>= put . f
