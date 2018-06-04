{-# LANGUAGE FlexibleContexts, GADTs, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Reader where

import Control.Effect
import Data.Functor.Classes (Show1(..))

data Reader context result where
  Ask :: Reader context context

ask :: Member (Reader context) effects => Eff effects context
ask = send Ask

local :: forall context effects a . Member (Reader context) effects => (context -> context) -> Eff effects a -> Eff effects a
local f m = do
  context <- ask
  let context' = f context
      bind :: Reader context result -> (result -> Eff effects b) -> Eff effects b
      bind Ask yield = yield context'
  interpose pure bind m


runReader :: (effects \\ S (Reader context)) rest => context -> Eff effects a -> Eff rest a
runReader context = relayEffect pure (\ Ask yield -> yield context)


deriving instance Show (Reader context result)

instance Show1 (Reader context) where
  liftShowsPrec _ _ = showsPrec
