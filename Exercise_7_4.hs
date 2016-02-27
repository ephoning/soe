{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_4 where

import Prelude.Unicode
import Tree

treeZip ∷ InternalTree a → InternalTree b → InternalTree (a,b)
treeZip ILeaf _ = ILeaf
treeZip _ ILeaf = ILeaf
treeZip (IBranch x t1 t2) (IBranch y t3 t4) = IBranch (x,y) (treeZip t1 t3) (treeZip t2 t4)

treeZipWith ∷ (a → b → c) → InternalTree a → InternalTree b → InternalTree c
treeZipWith f ILeaf _ = ILeaf
treeZipWith f _ ILeaf = ILeaf
treeZipWith f (IBranch x t1 t2) (IBranch y t3 t4) =
  IBranch (f x y) (treeZipWith f t1 t3) (treeZipWith f t2 t4)

