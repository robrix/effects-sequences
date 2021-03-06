{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Writer where

import Control.Effect

data Writer trace result where
  Tell :: trace -> Writer trace ()

tell :: Member (Writer trace) effects => trace -> Effect effects ()
tell = send . Tell


runWriter :: (Monoid trace, (effects \\ S (Writer trace)) rest) => Effect effects a -> Effect rest (a, trace)
runWriter = interpretStatefulEffect mempty (\ state a -> pure (a, state)) (\ traces (Tell trace) yield -> yield (traces <> trace) ())
