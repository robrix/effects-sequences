{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Effect.Higher.Functor where

type f ~> g = forall a . f a -> g a

class HFunctor f where
  hfmap :: (a ~> b) -> (f a ~> f b)
