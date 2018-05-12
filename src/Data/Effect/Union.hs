{-# LANGUAGE ExistentialQuantification #-}
module Data.Effect.Union where

data Union members a = forall member . Union !Int (member a)
