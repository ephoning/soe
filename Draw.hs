{-# LANGUAGE UnicodeSyntax #-}

module Draw (inchToPixel, pixelToInch, intToFloat,
             xWin, yWin, trans, shapeToGraphic, spaceClose) where

import Prelude.Unicode
import Graphics.SOE

import Shape

pixelsPerInch ∷ Int
pixelsPerInch = 100

inchToPixel ∷ Float → Int
inchToPixel x = round(x ⋅ intToFloat pixelsPerInch)

pixelToInch ∷ Int → Float
pixelToInch x = intToFloat x / 100

intToFloat ∷ Int → Float
intToFloat x = fromInteger (toInteger x)

xWin, yWin ∷ Int
xWin = 600
yWin = 500

trans ∷ Vertex → Point
trans (x,y) = (xWin2 + inchToPixel x,
               yWin2 - inchToPixel y)
xWin2, yWin2 ∷ Int
xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

transList ∷ [Vertex] → [Point]
transList [] = []
transList (v:vs) = trans v : transList vs

shapeToGraphic ∷ Shape → Graphic
shapeToGraphic (Rectangle s1 s2) =
  let s12 = s1 / 2
      s22 = s2 / 2
  in polygon (transList [(-s12, -s22),(-s12,s22),(s12,s22),(s12,-s22)])
shapeToGraphic (Ellipse r1 r2) =
  ellipse (trans (-r1,-r2)) (trans (r1,r2))
shapeToGraphic (RtTriangle s1 s2) =
  polygon( transList [(0,0),(s1,0),(0,s2)])
shapeToGraphic (Polygon vs) =
  polygon (transList vs)

  
spaceClose ∷ Window → IO ()
spaceClose w = do k ← getKey w
                  if k ≡ ' ' then closeWindow w
                             else spaceClose w

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
shapes = [sh1,sh2,sh3]
colors = cycle [Blue,Red,Green,Yellow,Magenta]
type ColoredShapes = [(Color,Shape)]
coloredShapes = zip colors shapes

drawColoredShapes :: Window -> ColoredShapes -> IO ()
drawColoredShapes w cs =
  sequence_ (map diw cs)
  where
    diw (c,s) = drawInWindow w (withColor c (shapeToGraphic s))
    
main = runGraphics (
  do w ← openWindow "First Window" (xWin,yWin)
     drawColoredShapes w coloredShapes 
     spaceClose w
  )
  
