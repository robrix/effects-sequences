module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, run
, runM
, interpose
-- * Unions
, Member
) where

import Control.Effect.Internal
import Data.Effect.Union
