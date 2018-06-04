{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect
import Control.Effect.Internal

runFail :: (effects \\ S Fail) rest => Eff effects a -> Eff rest (Either String a)
runFail = relayEffect (pure . Right) (\ (Fail s) _ -> pure (Left s))
