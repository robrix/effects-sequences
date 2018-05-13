module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, runM
, runSingleton
, runSingletonState
, interpose
-- * Unions
, Member
, Seq(..)
) where

import Control.Effect.Internal
import Data.Effect.Union
