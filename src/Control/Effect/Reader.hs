{-# LANGUAGE GADTs #-}
module Control.Effect.Reader where

data Reader context result where
  Reader :: Reader context context
