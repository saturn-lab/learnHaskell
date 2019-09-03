module Lec3 where

foo x y = x + y*3

bar x = foo x 7

copies k n = 
  if n == 0 then []
  else k : copies k (n-1)

{-

-- Bad Indentation:
copies2 k n = 
if n == 0 then []
else k : copies2 k (n-1)

-}

copies3 k n = 
  if n == 0 then 
    []
  else 
    k : copies3 k (n-1)
      
zap z = 
  let x = z
      y = z + z
  in 
    x + y
     
{-
badzap z =
  let x = z
  y = z + z  -- wrong indentation; uncomment to see error message
  in 
     x + y
-}

{-
badzap z =
  let 
    x = 
    z + 2       -- wrong indentation
    y = z + z 
  in 
  x + y
-}

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mycat :: [a] -> [a] -> [a]
mycat [] xs2 = xs2
mycat (x:xs) xs2 = x:(mycat xs xs2)