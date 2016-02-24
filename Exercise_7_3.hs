{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_3 where

import Prelude.Unicode

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a) deriving Show

-- treeFoldr ∷ (a → b → b) a → InternalTree b → b

-- treeRepeat ∷ a → InternalTree a

