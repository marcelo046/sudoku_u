module Grid
(takeValue
) where

takeValue :: [Int] -> Int -> Int -> Int -> Int -> Int
takeValue grid gx gy x y = (grid) !! (3*gx + 9*3*gy + x + 9*y)
