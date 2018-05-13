module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, run
, runM
, runSingleton
, interpose
-- * Unions
, Member
, Seq(..)
) where

import Control.Effect.Internal
import Data.Effect.Union
