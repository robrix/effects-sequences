{-# LANGUAGE ExplicitNamespaces #-}
module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, run
, runM
, handleEffect
, handleStatefulEffect
, interpretEffect
, interpretStatefulEffect
, reinterpretEffect
, handleEffects
, handleStatefulEffects
, interpretEffects
, interpretStatefulEffects
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
