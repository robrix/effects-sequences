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
, reinterpretEffect
, handleEffects
, handleStatefulEffects
, interpretEffects
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
, decompose
, inject
, project
) where

import Control.Effect.Internal
import Data.Effect.Union
