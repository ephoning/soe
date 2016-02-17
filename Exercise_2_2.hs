{-# LANGUAGE UnicodeSyntax #-}

module Exercise_2_1 where

import Shape

regularPolygon ∷ Int → Side → Shape
regularPolygon n s =
  let angle = 360 `div ` n in
    Polygon [(0.0,0.0)]
    
    
