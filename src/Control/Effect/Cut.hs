{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}
module Control.Effect.Cut where

import Control.Effect
import Control.Effect.Internal
import Control.Effect.Nondeterminism
import Data.Effect.Union

data Cut result = Cut
-- 
-- cutfail :: Member Cut effects => Eff effects a
-- cutfail = send Cut
--
-- skip :: Applicative f => f ()
-- skip = pure ()
--
-- cut :: (Member Cut effects, Member Nondeterminism effects) => Eff effects ()
-- cut = skip <|> cutfail
--
-- call :: (Member Nondeterminism rest, (effects \\ S Cut) rest) => Eff effects a -> Eff rest a
-- call = loop empty
--   where loop :: (Member Nondeterminism rest, (effects \\ S Cut) rest) => Eff rest a -> Eff effects a -> Eff rest a
--         loop q (Pure a) = pure a <|> q
--         loop q (Eff u queue) = case strengthenSingleton <$> delete u of
--           Left u'
--             | Just Zero <- project u' -> q
--             | Just Plus <- project u' -> loop (loop q (dequeue queue False)) (dequeue queue True)
--             | otherwise               -> Eff u' (unit (Arrow (loop q . dequeue queue)))
--           Right Cut                   -> empty
