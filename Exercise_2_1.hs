{-# LANGUAGE UnicodeSyntax #-}

module Exercise_2_1 where

import Shape

rectangle s1 s2 =
  Polygon (0,0) : (0,s1) : (s1,s2) : (s2,0) : []

rtTriangle s1 s2 =
  Polygon (0,0) : (0,s1) : (s2,0) : []
  
