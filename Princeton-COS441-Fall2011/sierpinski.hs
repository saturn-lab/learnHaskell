-- Sierpinski Triangle                                   --
-- Copied from Paul Hudak's Haskell School of Expression --

import SOE

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size =
  drawInWindow w (withColor Blue 
                    (polygon [(x,y), (x + size, y), (x, y - size)]))

minSize :: Int
minSize = 8

triangle :: Window -> Int -> Int -> Int -> IO ()
triangle w x y size =
  if size <= minSize then 
    fillTri w x y size 
  else 
    let size2 = size `div` 2 in
    do triangle w x y size2
       triangle w x (y - size2) size2
       triangle w (x + size2) y size2

spaceClose :: Window -> IO ()
spaceClose w =
  do k <- getKey w
     if k == ' ' then closeWindow w
 		 else spaceClose w

main =
  runGraphics $ do
    w <- openWindow "Sierpinski's Triangle" (360, 360)
    triangle w 50 300 256
    spaceClose w