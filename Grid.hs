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
,lerArq
,rList
{-,coluna
,linha
,bloco
,procuraBloco
,procuraLinha
,procuraColuna
,procuraCell
,impossiveis
,ehZero
,possiveis
,resolveCelula
,resolveSudoku-}
) where

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
--
invalido x y num oldGrid
    | not(num `elem` [1..9]) = True
    | not(x `elem` [1..9])   = True
    | not(y `elem` [1..9])   = True
    | num `elem` pegaBloco x y oldGrid   = True
    | num `elem` pegaLinha y grid 0 = True
    | num `elem` pegaColuna x grid 0 = True
    | otherwise              = False

--montarGrid :: Int -> Int -> Int -> [Int] -> [Int]
montarGrid x y num oldGrid =  if invalido x y num oldGrid then oldGrid
                              else take (x + 9*y) oldGrid ++ [num] ++ drop (x + 9*y + 1) oldGrid

-- verifica de ja preencheu todo o grid
venceu newGrid = product newGrid > 0

-- Pega um valor de um grid da coord x y
pegarValor :: [Int] -> Int -> Int -> Int
pegarValor grid x y = (grid) !! (x + 9*y)

lerNumero::IO Int
lerNumero = do
    num <- getLine
    return (read num)

--pegaLinha :: Int -> [Int] -> [Int]
pegaLinha _ _ 9 = []
pegaLinha n grid x = [grid !! (x + 9*n)] ++ pegaLinha n grid (x+1)

--pegaColuna :: Int -> [Int] -> [Int]
pegaColuna _ _ 9 = []
pegaColuna n grid x = [grid !! (9*x + n)] ++ pegaColuna n grid (x+1)

-- pega um bloco em forma de vetor em que o valor da coord x y está localizada
--pegaBloco :: Int -> Int -> [Int] -> [Int]
pegaBloco x y grid =  (take 3 (pegaLinha l grid c)) ++ (take 3 (pegaLinha (l+1) grid c)) ++ (take 3 (pegaLinha (l+2) grid c))
                    where l = (quot y 3)*3
                          c = (quot x 3)*3

printarGrid :: [Int] -> IO ()
printarGrid oldGrid = do
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

-------------  funções não sendo usadas  --------------------

lerArq :: FilePath -> IO ()
lerArq path = do xs <- readFile path
                 ys <- rList xs
                 print(ys)

rList :: String -> IO [Int]
rList = readIO

{-
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

impossiveis :: Int -> [Int] -> [Int]
impossiveis n puzzle = if zero then getImp else [1..9]
                     where zero = ehZero n puzzle
                           getImp = sort $ nub $ filter (/= 0) (getValues puzzle (procuraCell n) 0)
                                  where getValues [] _ _         = []
                                        getValues (x:xs) cells i = if i `elem` cells then x : nextVal else nextVal
                                                                 where nextVal = getValues xs cells (i + 1)

ehZero :: Int -> [Int] -> Bool
ehZero n puzzle = zero puzzle 0
                where zero _ 81      = error "fora do limite"
                      zero [] _      = True
                      zero (x:xs) i  = if i == n then x == 0 else zero xs (i + 1)

possiveis :: Int -> [Int] -> [Int]
possiveis n puzzle = delete [1..9] (impossiveis n puzzle)
                   where delete [] _       = []
                         delete (x:xs) imp = if not (elem x imp) then x : proxDel else proxDel
                                           where proxDel = delete xs imp

resolveCelula :: Int -> [Int] -> Int
resolveCelula n puzzle = resolve (possiveis n puzzle)
                   where resolve [] = 0
                         resolve (v:vs) | not (v `elem` (pos procuraBloco))  = v
                                      | not (v `elem` (pos procuraLinha))    = v
                                      | not (v `elem` (pos procuraColuna)) = v
                                      | otherwise                         = resolve vs
                         pos f = pos (filter (/= n) (f n))
                               where pos []      = []
                                     pos (x:xs)  = possiveis x puzzle ++ pos xs

resolveSudoku :: [Int]
resolveSudoku = resolve grid 0
              where resolve puzzle 5000 = puzzle
                    resolve puzzle i = if all (/= 0) puzzle
                                     then puzzle
                                     else resolve (loop puzzle puzzle 0) (i + 1)

                    loop _ [] _          = []
                    loop puzzle (0:xs) n = resolveCelula n puzzle : loop puzzle xs (n + 1)
                    loop puzzle (x:xs) n = x : loop puzzle xs (n + 1)
-}
