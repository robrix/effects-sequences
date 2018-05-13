{-# LANGUAGE FlexibleContexts, GADTs #-}
module Control.Effect.Writer where

import Control.Effect

data Writer trace result where
  Tell :: trace -> Writer trace ()

tell :: Member (Writer trace) effects => trace -> Effect effects ()
tell = send . Tell
