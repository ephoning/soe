{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_3 where

import Tree
import Prelude.Unicode

treeRepeat ∷ a → InternalTree a
treeRepeat x = IBranch x (treeRepeat x) (treeRepeat x)

treeFoldr ∷ (a → b → b) → b → InternalTree a → b
treeFoldr f a t = foldr f a (treeWalkIn t)

treeFoldl ∷ (a → b → a) → a → InternalTree b → a
treeFoldl f a t = foldl f a (treeWalkIn t)

