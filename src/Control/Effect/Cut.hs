{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Cut where

import Control.Effect

data Cut result = Cut

cutfail :: Member Cut effects => Effect effects a
cutfail = send Cut

skip :: Applicative f => f ()
skip = pure ()
