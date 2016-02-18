{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_7 where

addPairsPointWise ∷ [(Integer,Integer)] → (Integer,Integer)
addPairsPointWise ps = addPairsPointWise' ps (0,0) where
  addPairsPointWise' [] acc = acc
  addPairsPointWise' ((x,y) : ps) (x',y') = addPairsPointWise' ps (x + x',y+y')
  
