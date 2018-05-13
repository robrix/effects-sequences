{-# LANGUAGE DataKinds #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect
import Control.Effect.Internal

runFail :: Effect ('S Fail) a -> Either String a
runFail = handleEffect Right (\ (Fail s) _ -> Left s)
