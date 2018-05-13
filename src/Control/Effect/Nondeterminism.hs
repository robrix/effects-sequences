{-# LANGUAGE DataKinds, FlexibleContexts, GADTs #-}
module Control.Effect.Nondeterminism
( Nondeterminism(..)
, msplit
, runNondeterminism
) where

import Control.Applicative
import Control.Effect
import Control.Effect.Internal
import Data.Effect.Union
import Data.Foldable (asum)

msplit :: Member Nondeterminism effects => Effect effects a -> Effect effects (Maybe (a, Effect effects a))
msplit = interposeSplit []
  (\ jq a -> pure (Just (a, asum jq)))
  (\ jq eff yield loop -> case eff of
    Zero -> case jq of
      []    -> pure Nothing
      j:jq' -> loop jq' j
    Plus -> loop (yield False : jq) (yield True))

runNondeterminism :: Alternative f => Effect ('S Nondeterminism) a -> f a
runNondeterminism = handleEffects pure (\ eff yield -> case strengthenSingleton eff of
  Zero -> empty
  Plus -> yield True <|> yield False)
