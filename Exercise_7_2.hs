{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_2 where

import Prelude.Unicode

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a) deriving Show

takeTree ∷ Int → InternalTree a → InternalTree a
takeTree _ ILeaf = ILeaf
takeTree 0 _ = ILeaf
takeTree n (IBranch x t1 t2) = IBranch x (takeTree (n-1) t1) (takeTree (n-1) t2)


takeTreeWhile ∷ (a → Bool) → InternalTree a → InternalTree a
takeTreeWhile p ILeaf = ILeaf
takeTreeWhile p (IBranch x t1 t2) = if p x
                                    then IBranch x (takeTreeWhile p t1) (takeTreeWhile p t2)
                                    else ILeaf
                                         

