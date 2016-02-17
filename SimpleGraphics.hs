{-# LANGUAGE UnicodeSyntax #-}

module SimpleGraphics where

import Prelude.Unicode
import Graphics.SOE

spaceClose ∷ Window → IO ()
spaceClose w = do k ← getKey w
                  if k ≡ ' ' then closeWindow w
                             else spaceClose w
                         
main1 = runGraphics (
  do w ← openWindow "First Window" (300,300)
     drawInWindow w (text (100,200) "Hello")
     spaceClose w
  )

pic1 = (withColor Blue (ellipse (150,150) (200,200)))
pic2 = (withColor Red (polyline [(100,50),(150,150),(200,200),(70,60)]))

main2 = runGraphics (
  do w ← openWindow "First Window" (300,300)
     drawInWindow w (text (100,200) "Hello")
     drawInWindow w pic1
     drawInWindow w pic2
     spaceClose w
  )
  

fillTri ∷ Color → Int → Int → Int → Graphic
fillTri c x y size =
  withColor c (polygon [(x,y),(x+size,y),(x,y-size),(x,y)])

minSize ∷ Int
minSize = 8

sierpinskiTri ∷ Window -> Color → Int → Int → Int → IO ()
sierpinskiTri w c x y size =
  if size ≤ minSize
     then drawInWindow w (fillTri c x y size)
     else let size2 = size `div` 2 in
          do sierpinskiTri w c x y size2
             sierpinskiTri w c (x+size2) y size2
             sierpinskiTri w c x (y-size2) size2

main3 = runGraphics (
  do w ← openWindow "First Window" (400,400)
     sierpinskiTri w Blue 50 300 256
     sierpinskiTri w Red 66 332 256
     sierpinskiTri w Green 96 284 256
     spaceClose w
  )

    
