{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_1 where

import Prelude.Unicode

data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- leaf values as list
fringe ∷ Tree a → [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2

-- number of leaves
treeSize ∷ Tree a → Integer
treeSize (Leaf x) = 1
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2


treeFold ∷ (a → b) → (b → b → b) → (Tree a) → b
treeFold lf bf (Leaf x) = lf x
treeFold lf bf (Branch t1 t2) = (treeFold lf bf t1) `bf` (treeFold lf bf t2)


