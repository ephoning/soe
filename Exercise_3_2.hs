{-# LANGUAGE UnicodeSyntax #-}

module SimpleGraphics where

import Prelude.Unicode
import Graphics.SOE

spaceClose ∷ Window → IO ()
spaceClose w = do k ← getKey w
                  if k ≡ ' ' then closeWindow w
                             else spaceClose w
                         
fillStarUp ∷ Color → Int → Int → Int → Graphic
fillStarUp c x y size =
  let hsize = size `div` 2 in
  withColor c (polygon [(x,y-size),(x+size,y+hsize),(x-size,y+hsize),(x,y-size)])
fillStarDown ∷ Color → Int → Int → Int → Graphic
fillStarDown c x y size =
  let hsize = size `div` 2 in
  withColor c (polygon [(x,y+size),(x+size,y-hsize),(x-size,y-hsize),(x,y+size)])

fillStar ∷ Window → Color → Int → Int → Int → IO ()
fillStar w c x y size = do
     drawInWindow w (fillStarUp c x y size)
     drawInWindow w (fillStarDown c x y size)
  
minSize ∷ Int
minSize = 1

colors = cycle [Red, Green, Blue, Magenta, Yellow]

snowflake ∷ Window → Int → Int → Int → Int → IO ()
snowflake w c x y size =
  let hsize = size `div` 2
      tsize = size `div` 3
      offset = hsize+tsize
      c' = c + 1 in
    do
      fillStar w (colors !! c) x y size -- center
      if hsize > minSize
        then do
          snowflake w c' x (y-offset) tsize -- satellites
          snowflake w c' (x+offset) (y+tsize) tsize
          snowflake w c' (x-offset) (y+tsize) tsize
          snowflake w c' x (y+offset) tsize
          snowflake w c' (x+offset) (y-tsize) tsize
          snowflake w c' (x-offset) (y-tsize) tsize
        else
          return ()
    
main = runGraphics (
  do w ← openWindow "First Window" (900,900)
     snowflake w 0 450 450 440
     spaceClose w
  )

    
