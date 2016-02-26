{-# LANGUAGE UnicodeSyntax #-}

module Tree where

import Prelude.Unicode

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a) deriving Show

-- in-order traversal
treeWalk ∷ InternalTree a → [a]
treeWalk ILeaf = []
treeWalk (IBranch x t1 t2) = x : (treeWalk t1) ++ (treeWalk t2)
