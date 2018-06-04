{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Nondeterminism
( Nondeterminism(..)
, msplit
, unmsplit
, bagofN
, ifte
, once
, runNondeterminism
, Alternative(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect
import Control.Effect.Internal
import Data.Foldable (asum)

msplit :: Member Nondeterminism effects => Effect effects a -> Effect effects (Maybe (a, Effect effects a))
msplit = interposeSplit []
  (\ jq a -> pure (Just (a, asum jq)))
  (\ jq eff yield loop -> case eff of
    Zero -> case jq of
      []    -> pure Nothing
      j:jq' -> loop jq' j
    Plus -> loop (yield False : jq) (yield True))

unmsplit :: Member Nondeterminism effects => Maybe (a, Effect effects a) -> Effect effects a
unmsplit = maybe empty (uncurry ((<|>) . pure))

bagofN :: Member Nondeterminism effects => Maybe Int -> Effect effects a -> Effect effects [a]
bagofN (Just n) _ | n <= 0 = pure []
bagofN n        m          = msplit m >>= go
  where go Nothing         = pure []
        go (Just (a, m'))  = (a:) <$> bagofN (pred <$> n) m'

ifte :: Member Nondeterminism effects => Effect effects a -> (a -> Effect effects b) -> Effect effects b -> Effect effects b
ifte cond then' else' = msplit cond >>= maybe else' (uncurry bind)
  where bind a rest = then' a <|> (rest >>= then')

once :: Member Nondeterminism effects => Effect effects a -> Effect effects a
once m = msplit m >>= maybe empty (pure . fst)


runNondeterminism :: (Alternative f, (effects \\ S Nondeterminism) rest) => Effect effects a -> Effect rest (f a)
runNondeterminism = relayEffect (pure . pure) (\ eff yield -> case eff of
  Zero -> pure empty
  Plus -> (<|>) <$> yield True <*> yield False)
