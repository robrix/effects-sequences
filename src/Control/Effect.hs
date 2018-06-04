{-# LANGUAGE ExplicitNamespaces #-}
module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, run
, runM
, relayEffect
, relayStatefulEffect
, reinterpretEffect
, relayEffects
, relayStatefulEffects
, reinterpretEffects
, interpose
, interposeState
, interposeSplit
-- * Unions
, Member
, Seq
, S
, Empty
, type (:+:)
, type (\\)
, type (>->)
, decompose
, inject
, project
) where

import Control.Effect.Internal
import Data.Effect.Union
