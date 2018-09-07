
import Data.List
import System.IO

let grid = [1..81]

--main :: IO ()
--main = do jogo([1..81])

--jogo :: [Int] -> [Int]
jogo xs = do  printarGrid [1..81]--oldGrid
              putStrLn("digite a coordenada x: ")
              x <- lerNumero
              putStrLn("digite a coordenada y: ")
              y <- lerNumero
              putStrLn("digite o valor: ")
              num <- lerNumero
              let newGrid = [1..81]--montarGrid x y num oldGrid
              if venceu([1,0,5]) then putStrLn("venceu!!!") else jogo(xs)--(newGrid)

venceu :: [Int] -> Bool
venceu xs = product xs > 0

lerNumero::IO Int
lerNumero = do
    num <- getLine
    return (read num)

coluna :: Int -> [Int]
coluna n = resto n 0
         where resto _ 9 = []
               resto n i = n : resto (n + 9) (i + 1)

linha :: Int -> [Int]
linha n = pontoinicio : resto pontoinicio 1
      where pontoinicio = (n * 9)
            resto _ 9 = []
            resto n i = n + i : resto n (i + 1)

bloco :: Int -> [Int]
bloco n = resto pontoinicio 0 0 0
  where pontoinicio | n <= 2 = n * 3
                   | n <= 5 = 27 + (n * 3 - 9)
                   | n <= 8 = 54 + (n * 3 - 18)
        resto _ _ _ 9 = []
        resto pontoinicio x n i = numero : resto pontoinicio proxX proxN (i + 1)
                              where numero = pontoinicio + n + x
                                    proxX = if n == 2 then x + 9 else x
                                    proxN = if n == 2 then 0 else n + 1

procuraBloco :: Int -> [Int]
procuraBloco x = procura x 0
              where procura _ 9 = error "Bloco nao encontrado"
                    procura x n = if x `elem` blk then blk else procura x (n + 1)
                               where blk = bloco n

procuraLinha :: Int -> [Int]
procuraLinha x = procura x 0
            where procura _ 9 = error "Linha nao encontrada"
                  procura x n = if x `elem` rw then rw else procura x (n + 1)
                             where rw = linha n

procuraColuna :: Int -> [Int]
procuraColuna x = procura x 0
               where procura _ 9 = error "Coluna nao encontrada"
                     procura x n = if x `elem` col then col else procura x (n + 1)
                                where col = coluna n

procuraCell :: Int -> [Int]
procuraCell n = sort $ nub $ rw ++ col ++ blk
             where rw  = procuraLinha n
                   col = procuraColuna n
                   blk = procuraBloco n

printarGrid :: [Int] -> IO ()
printarGrid oldGrid = do
              print linhaVertical
              --meuprint resolveSudoku 0
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
