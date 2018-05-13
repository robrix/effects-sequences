module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, run
, runM
-- * Unions
, Member
) where

import Control.Effect.Internal
import Data.Effect.Union
