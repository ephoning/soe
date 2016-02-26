{-# LANGUAGE UnicodeSyntax #-}

module Tree where

import Prelude.Unicode

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a) deriving Show

-- pre-order traversal
treeWalkPre ∷ InternalTree a → [a]
treeWalkPre ILeaf = []
treeWalkPre (IBranch x t1 t2) = x : (treeWalkPre t1) ++ (treeWalkPre t2)

-- in-order traversal
treeWalkIn ∷ InternalTree a → [a]
treeWalkIn ILeaf = []
treeWalkIn (IBranch x t1 t2) = (treeWalkIn t1) ++ [x] ++ (treeWalkIn t2)

-- post-order traversal
treeWalkPost ∷ InternalTree a → [a]
treeWalkPost ILeaf = []
treeWalkPost (IBranch x t1 t2) = (treeWalkPost t1) ++ (treeWalkPost t2) ++ [x]
