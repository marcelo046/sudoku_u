import Data.List
import System.IO
import Grid


main = do printarGrid
          putStrLn "digite a coordenada x: "
          x <- getNumber
          putStrLn "digite a coordenada y: "
          y <- getNumber
          putStrLn "digite o valor: "
          num <- getNumber
          marcarNoGrid x y num -- terminar essa função
          -- depois colocar aqui função de valiar
          if venceu then putStrLn "venceu!!!" else main -- no lugar do False, colocar função para ver se venceu


--let venceu = False

rList :: String -> IO [Int]
rList = readIO

















{- exemplo de grid 9 x 9 (para teste, sudoku real nros são de 1 a 9)
| 1  2  3|  4  5  6|  7  8  9|
|10 11 12| 13 14 15| 16 17 18|
|19 20 21| 22 23 24| 25 26 27|
------------------------------
|28 29 30| 31 32 33| 34 35 36|
|37 38 39| 40 41 42| 43 44 45|
|46 47 48| 49 50 51| 52 53 54|
------------------------------
|55 56 57| 58 59 60| 61 62 63|
|64 65 66| 67 68 69| 70 71 72|
|73 74 75| 76 77 78| 79 80 81|
-}

{-printa :: (Num b) => [a] -> b
printa :: Show a => [a] -> IO ()
printa [] = print(" fim")
printa (x:xs) = do  print(x)
                    printa(xs)
-}
