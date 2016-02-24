{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_1 where

import Prelude.Unicode
import Shape

area ∷ Shape → Float
area (Rectangle s1 s2) = s1 ⋅ s2
area (RtTriangle s1 s2) = s1 ⋅ s2 / 2
area (Ellipse r1 r2) = π ⋅ r1 ⋅ r2
area (Polygon (v1 : vs)) = polyArea vs
  where polyArea ∷ [Vertex] → Float
        polyArea ls = foldl (+) 0 (map triArea (triples v1 vs))
          where triples v1 ls = map (\ (v2,v3) -> (v1,v2,v3)) (zip vs (tail vs))

triArea ∷ (Vertex,Vertex,Vertex) → Float
triArea (v1,v2,v3) = let a = distBetween v1 v2
                         b = distBetween v2 v3
                         c = distBetween v3 v1
                         s = 0.5 ⋅ (a + b + c)
                     in sqrt(s ⋅ (s - a) ⋅ (s - b) * (s - c))

