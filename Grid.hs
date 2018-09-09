module Grid
(grid
,invalido
,montarGrid
,venceu
,pegarValor
,lerNumero
,pegaLinha
,pegaColuna
,pegaBloco
,printarGrid
) where

import System.IO

grid :: [Int]
grid = [5, 3, 0,  0, 7, 0,  0, 0, 0,
        6, 0, 0,  1, 9, 5,  0, 0, 0,
        0, 9, 8,  0, 0, 0,  0, 6, 0,

        8, 0, 0,  0, 6, 0,  0, 0, 3,
        4, 0, 0,  8, 0, 3,  0, 0, 1,
        7, 0, 0,  0, 2, 0,  0, 0, 6,

        0, 6, 0,  0, 0, 0,  2, 8, 0,
        0, 0, 0,  4, 1, 9,  0, 0, 5,
        0, 0, 0,  0, 8, 0,  0, 7, 9]

invalido :: Int -> Int -> Int -> [Int] -> Bool
invalido x y num oldGrid
    | num == -1 = False
    | not(num `elem` [0..9]) = True
    | not(x `elem` [0..8])   = True
    | not(y `elem` [0..8])   = True
    | (pegarValor grid x y) /= 0 = True
    | num == 0 = False
    | num `elem` pegaBloco x y oldGrid   = True
    | num `elem` pegaLinha y grid 0 = True
    | num `elem` pegaColuna x grid 0 = True
    | otherwise              = False

montarGrid :: Int -> Int -> Int -> [Int] -> [Int]
montarGrid x y num oldGrid =  if invalido x y num oldGrid then oldGrid
                              else if num == -1 then grid
                                   else take (x + 9*y) oldGrid ++ [num] ++ drop (x + 9*y + 1) oldGrid

-- verifica de ja preencheu todo o grid
venceu :: [Int] -> Bool
venceu [] = True
venceu newGrid = if (take 1 newGrid) == [0] then False else venceu (drop 1 newGrid)

-- Pega um valor de um grid da coord x y
pegarValor :: [Int] -> Int -> Int -> Int
pegarValor grid x y = (grid) !! (x + 9*y)

lerNumero::IO Int
lerNumero = do
    num <- getLine
    return (read num)

pegaLinha :: Int -> [Int] -> Int -> [Int]
pegaLinha _ _ 9 = []
pegaLinha n grid x = [grid !! (x + 9*n)] ++ pegaLinha n grid (x+1)

pegaColuna :: Int -> [Int] -> Int -> [Int]
pegaColuna _ _ 9 = []
pegaColuna n grid x = [grid !! (9*x + n)] ++ pegaColuna n grid (x+1)

-- pega um bloco em forma de vetor em que o valor da coord x y está localizada
pegaBloco :: Int -> Int -> [Int] -> [Int]
pegaBloco x y grid =  (take 3 (pegaLinha l grid c)) ++ (take 3 (pegaLinha (l+1) grid c)) ++ (take 3 (pegaLinha (l+2) grid c))
                    where l = (quot y 3)*3
                          c = (quot x 3)*3

printarGrid :: [Int] -> IO ()
printarGrid oldGrid = do
              putStrLn "\n"
              print linhaVertical
              meuprint oldGrid 0
            where meuprint [] _   = do
                                   print linhaVertical
                  meuprint oldGrid 3 = do
                                   print linhaVertical
                                   meuprint oldGrid 0
                  meuprint oldGrid n = do
                                   print (linha (take 9 oldGrid) "|" 0)
                                   meuprint (drop 9 oldGrid) (n + 1)

                  linha [] str _     = str ++ " |"
                  linha x  str 3     = linha x (str ++ " |") 0
                  linha (x:xs) str n = linha xs (str ++ " " ++ (show x)) (n + 1)

                  linhaVertical = replicate 25 '-'


{-grid :: [Int]
grid = [5, 0, 0,  6, 7, 8,  9, 1, 2,
        6, 7, 2,  1, 9, 5,  3, 4, 8,
        1, 9, 8,  3, 4, 2,  5, 6, 7,

        8, 5, 9,  7, 6, 1,  4, 2, 3,
        4, 2, 6,  8, 5, 3,  7, 9, 1,
        7, 1, 3,  9, 2, 4,  8, 5, 6,

        9, 6, 1,  5, 3, 7,  2, 8, 4,
        2, 8, 7,  4, 1, 9,  6, 3, 5,
        3, 4, 5,  2, 8, 6,  1, 7, 9]-}
{-                Resposta
        5, 3, 4,  6, 7, 8,  9, 1, 2,
        6, 7, 2,  1, 9, 5,  3, 4, 8,
        1, 9, 8,  3, 4, 2,  5, 6, 7,

        8, 5, 9,  7, 6, 1,  4, 2, 3,
        4, 2, 6,  8, 5, 3,  7, 9, 1,
        7, 1, 3,  9, 2, 4,  8, 5, 6,

        9, 6, 1,  5, 3, 7,  2, 8, 4,
        2, 8, 7,  4, 1, 9,  6, 3, 5,
        3, 4, 5,  2, 8, 6,  1, 7, 9,
-}
