{-# LANGUAGE ExplicitNamespaces #-}
module Control.Effect
( Effect
-- * Constructing effects
, send
-- * Handlers
, runM
, handleEffect
, handleStatefulEffect
, Handle(..)
, interpose
, interposeState
, interposeSplit
-- * Unions
, Member
, Seq
, S
, type (:+:)
, decompose
, inject
, project
) where

import Control.Effect.Internal
import Data.Effect.Union
