{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, StandaloneDeriving #-}
module Control.Effect.Reader where

import Control.Effect
import Data.Functor.Classes (Show1(..))

data Reader context result where
  Reader :: Reader context context

ask :: Member (Reader context) effects => Effect effects context
ask = send Reader

local :: forall context effects a . Member (Reader context) effects => (context -> context) -> Effect effects a -> Effect effects a
local f m = do
  context <- ask
  let context' = f context
      bind :: Reader context result -> (result -> Effect effects b) -> Effect effects b
      bind Reader yield = yield context'
  interpose pure bind m


runReader :: context -> Effect ('S (Reader context)) a -> a
runReader context = handleEffect id (\ Reader yield -> yield context)


deriving instance Show (Reader context result)

instance Show1 (Reader context) where
  liftShowsPrec _ _ = showsPrec
