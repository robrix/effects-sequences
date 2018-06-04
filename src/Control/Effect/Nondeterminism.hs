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

msplit :: Member Nondeterminism effects => Eff effects a -> Eff effects (Maybe (a, Eff effects a))
msplit = interposeSplit []
  (\ jq a -> pure (Just (a, asum jq)))
  (\ jq eff yield loop -> case eff of
    Zero -> case jq of
      []    -> pure Nothing
      j:jq' -> loop jq' j
    Plus -> loop (yield False : jq) (yield True))

unmsplit :: Member Nondeterminism effects => Maybe (a, Eff effects a) -> Eff effects a
unmsplit = maybe empty (uncurry ((<|>) . pure))

bagofN :: Member Nondeterminism effects => Maybe Int -> Eff effects a -> Eff effects [a]
bagofN (Just n) _ | n <= 0 = pure []
bagofN n        m          = msplit m >>= go
  where go Nothing         = pure []
        go (Just (a, m'))  = (a:) <$> bagofN (pred <$> n) m'

ifte :: Member Nondeterminism effects => Eff effects a -> (a -> Eff effects b) -> Eff effects b -> Eff effects b
ifte cond then' else' = msplit cond >>= maybe else' (uncurry bind)
  where bind a rest = then' a <|> (rest >>= then')

once :: Member Nondeterminism effects => Eff effects a -> Eff effects a
once m = msplit m >>= maybe empty (pure . fst)


runNondeterminism :: (Alternative f, (effects \\ S Nondeterminism) rest) => Eff effects a -> Eff rest (f a)
runNondeterminism = relayEffect (pure . pure) (\ eff yield -> case eff of
  Zero -> pure empty
  Plus -> (<|>) <$> yield True <*> yield False)
