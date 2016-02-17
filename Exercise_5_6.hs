{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_6 where

maxList0 ∷ [Integer] → Integer
maxList0 l = maxList' l 0 where
  maxList' [] max = max
  maxList' (x:xs) max = if x > max
                        then maxList' xs x
                        else maxList' xs max

maxList ∷ [Integer] → Integer
maxList l = pList l (>) 0

minList ∷ [Integer] → Integer
minList l = pList l (<) 100000000

pList ∷ [Integer] → (Integer → Integer → Bool) → Integer → Integer
pList [] p acc = acc
pList (x:xs) p acc = if x `p` acc
                     then pList xs p x
                     else pList xs p acc
