{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_5 where

doubleEach ∷ [Integer] → [Integer]
doubleEach = map (*2)

pairAndOne ∷ [Integer] → [(Integer,Integer)]
pairAndOne = map (\ x → (x,x+1))

addEachPair ∷ [(Integer,Integer)] → [Integer]
addEachPair = map (\ (x,y) → x+y)
