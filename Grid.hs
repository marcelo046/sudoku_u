module Grid
(takeValue
,lerArq
) where

-- deve-se colocar esses imports, mesmo tendo na main
import System.IO
import Data.List

-- Pega um valor de um grid
takeValue :: [Int] -> Int -> Int -> Int -> Int -> Int
takeValue grid gx gy x y = (grid) !! (3*gx + 9*3*gy + x + 9*y)

-- funções começam com letra minúscula
lerArq :: FilePath -> IO ()
lerArq path = do xs <- readFile path -- listas ficam no plural (xs e não só x)
                 ys <- rList xs
                 print(ys)

rList :: String -> IO [Int]
rList = readIO





















-- Verifica se tem algum valor repitido (ignora o 0)
--MyRepeat :: (Eq a) => [a] -> Bool


--printGrid h:t = do



{-
x <- readFile "testeLer.txt"
          y <- rList x
          print( y )

rList :: String -> IO [Int]
rList = readIO
-}




















{-MyRepeat [] = False
MyRepeat [_] = False
MyRepeat (h:t) = if h /= 0 && elem h t then True
                                        else MyRepeat t-}
