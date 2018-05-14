{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Control.Effect.Fail
( Fail(..)
, runFail
) where

import Control.Effect
import Control.Effect.Internal

runFail :: (effects \\ S Fail) rest => Effect effects a -> Effect rest (Either String a)
runFail = interpretEffect (pure . Right) (\ (Fail s) _ -> pure (Left s))
