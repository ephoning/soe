{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_9 where

import Prelude.Unicode

makeChange ∷ Int → [Int] → [Int]
makeChange amt denoms = makeChange' amt denoms [] where
  makeChange' 0 _ change = reverse change
  makeChange' amt (denom:denoms) change =
    let coins = (amt `div` denom) in        
    if coins > 0
       then makeChange' (amt - coins ⋅ denom) denoms (coins:change)
       else makeChange' amt denoms change
  
