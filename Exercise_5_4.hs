{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_4 where

pam ∷ [a → b] → a → [b]
pam [] x = []
pam (f:fs) x = f x : pam fs x

-- pam (map (*) [1,2,3,4]) 5

f1 (op,l) a = map (op a) l
f2 op l = (op,l)

-- f1 (f2 (*) [1,2,3,4]) 5
