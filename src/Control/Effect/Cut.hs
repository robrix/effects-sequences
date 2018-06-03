{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Cut where

import Control.Effect
import Control.Effect.Internal
import Control.Effect.Nondeterminism
import Data.Effect.Union

data Cut result = Cut

cutfail :: Member Cut effects => Effect effects a
cutfail = send Cut

skip :: Applicative f => f ()
skip = pure ()

cut :: (Member Cut effects, Member Nondeterminism effects) => Effect effects ()
cut = skip <|> cutfail

call :: (Member Nondeterminism rest, (effects \\ S Cut) rest) => Effect effects a -> Effect rest a
call = loop empty
  where loop :: (Member Nondeterminism rest, (effects \\ S Cut) rest) => Effect rest a -> Effect effects a -> Effect rest a
        loop q (Pure a) = pure a <|> q
        loop q (Effect u queue) = case strengthenSingleton <$> delete u of
          Left u'
            | Just Zero <- project u' -> q
            | Just Plus <- project u' -> loop (loop q (dequeue queue False)) (dequeue queue True)
            | otherwise               -> Effect u' (unit (Arrow (loop q . dequeue queue)))
          Right Cut                   -> empty
