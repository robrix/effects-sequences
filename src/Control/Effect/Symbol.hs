{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Symbol where

import Control.Applicative (Alternative(..))
import Control.Effect
import Control.Effect.Nondeterminism

data Symbol return where
  Satisfy :: (Char -> Bool) -> Symbol Char

parse :: (Member Nondeterminism rest, (effects \\ S Symbol) rest) => String -> Effect effects a -> Effect rest a
parse ts = interpretStatefulEffect ts (\ ts a -> if null ts then pure a else empty) (\ ts eff yield -> case eff of
  Satisfy predicate -> case ts of
    t:ts | predicate t -> yield ts t
    _ -> empty)
