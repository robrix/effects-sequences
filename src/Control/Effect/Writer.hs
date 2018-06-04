{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Writer where

import Control.Effect

data Writer trace result where
  Tell :: trace -> Writer trace ()

tell :: Member (Writer trace) effects => trace -> Eff effects ()
tell = send . Tell


runWriter :: (Monoid trace, (effects \\ S (Writer trace)) rest) => Eff effects a -> Eff rest (a, trace)
runWriter = relayStatefulEffect mempty (\ state a -> pure (a, state)) (\ traces (Tell trace) yield -> yield (traces <> trace) ())
