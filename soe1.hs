module Eddie where

import Graphics.SOE

main = runGraphics (
  do
    w <- openWindow "Hello World" (300,300)
    drawInWindow w (text (100,200) "Hello")
    k <- getKey w
    closeWindow w
  )
