module Data.Effect.BinaryTree where

data BinaryTree a = Z | L a | BinaryTree a :+: BinaryTree a
