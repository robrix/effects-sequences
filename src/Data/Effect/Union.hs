{-# LANGUAGE ExistentialQuantification #-}
module Data.Effect.Union where

data Union members a = forall member . Union {-# UNPACK #-} !Int (member a)
