{-# LANGUAGE FlexibleContexts, GADTs #-}
module Control.Effect.Writer where

import Control.Effect

data Writer trace result where
  Tell :: trace -> Writer trace ()

tell :: Member (Writer trace) effects => trace -> Effect effects ()
tell = send . Tell


runWriter :: Monoid trace => Effect (S (Writer trace)) a -> (a, trace)
runWriter = handleStatefulEffect mempty (flip (,)) (\ traces (Tell trace) yield -> yield (traces <> trace) ())
