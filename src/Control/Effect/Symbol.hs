{-# LANGUAGE GADTs #-}
module Control.Effect.Symbol where

data Symbol token return where
  Satisfy :: (token -> Bool) -> Symbol token token
