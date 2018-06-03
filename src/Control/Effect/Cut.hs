{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Cut where

import Control.Effect
import Control.Effect.Nondeterminism

data Cut result = Cut

cutfail :: Member Cut effects => Effect effects a
cutfail = send Cut

skip :: Applicative f => f ()
skip = pure ()

cut :: (Member Cut effects, Member Nondeterminism effects) => Effect effects ()
cut = skip <|> cutfail
