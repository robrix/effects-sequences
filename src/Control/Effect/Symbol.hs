{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Symbol where

import Control.Effect
import Control.Effect.Nondeterminism

data Symbol return where
  Satisfy :: (Char -> Bool) -> Symbol Char

char :: Member Symbol effects => Char -> Eff effects Char
char c = send (Satisfy (== c))

parse :: (Member Nondeterminism rest, (effects \\ S Symbol) rest) => String -> Eff effects a -> Eff rest a
parse ts = relayStatefulEffect ts (\ ts a -> if null ts then pure a else empty) (\ ts eff yield -> case eff of
  Satisfy predicate -> case ts of
    t:ts | predicate t -> yield ts t
    _ -> empty)
