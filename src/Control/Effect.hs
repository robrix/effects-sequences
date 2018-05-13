module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, runM
, Handle(..)
, runSingletonState
, interpose
, interposeState
, interposeSplit
-- * Unions
, Member
, Seq(..)
) where

import Control.Effect.Internal
import Data.Effect.Union
