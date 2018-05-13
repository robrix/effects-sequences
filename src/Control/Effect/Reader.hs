{-# LANGUAGE FlexibleContexts, GADTs, StandaloneDeriving #-}
module Control.Effect.Reader where

import Control.Effect

data Reader context result where
  Reader :: Reader context context

ask :: Member (Reader context) effects => Effect effects context
ask = send Reader


deriving instance Show (Reader context result)
