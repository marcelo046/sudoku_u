
import Data.List
import System.IO

{-let grid = [5, 3, 0,  0, 7, 0,  0, 0, 0,
        6, 0, 0,  1, 9, 5,  0, 0, 0,
        0, 9, 8,  0, 0, 0,  0, 6, 0,

        8, 0, 0,  0, 6, 0,  0, 0, 3,
        4, 0, 0,  8, 0, 3,  0, 0, 1,
        7, 0, 0,  0, 2, 0,  0, 0, 6,

        0, 6, 0,  0, 0, 0,  2, 8, 0,
        0, 0, 0,  4, 1, 9,  0, 0, 5,
        0, 0, 0,  0, 8, 0,  0, 7, 9]-}
--let grid = [1..81]
--import Grid

main = jogo([1..81])

jogo oldGrid = do printarGrid oldGrid
                  putStrLn("digite a coordenada x: ")
                  x <- getNumber
                  putStrLn "digite a coordenada y: "
                  y <- getNumber
                  putStrLn "digite o valor: "
                  num <- getNumber
                  let newGrid = montarGrid x y num oldGrid
                  if venceu newGrid then putStrLn "venceu!!!" else jogo newGrid

--grid = [1..81]
montarGrid x y num oldGrid = [1..81]

venceu newGrid = product newGrid > 0

-- Pega um valor de um grid
pegarValor :: [Int] -> Int -> Int -> Int -> Int -> Int
pegarValor grid gx gy x y = (grid) !! (3*gx + 9*3*gy + x + 9*y)

getNumber::IO Int
getNumber = do
    num <- getLine
    return (read num)

-- funções começam com letra minúscula
lerArq :: FilePath -> IO ()
lerArq path = do xs <- readFile path -- listas ficam no plural (xs e não só x)
                 ys <- rList xs
                 print(ys)

rList :: String -> IO [Int]
rList = readIO

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

{-let grid = [5, 3, 0,  0, 7, 0,  0, 0, 0,
       6, 0, 0,  1, 9, 5,  0, 0, 0,
       0, 9, 8,  0, 0, 0,  0, 6, 0,

       8, 0, 0,  0, 6, 0,  0, 0, 3,
       4, 0, 0,  8, 0, 3,  0, 0, 1,
       7, 0, 0,  0, 2, 0,  0, 0, 6,

       0, 6, 0,  0, 0, 0,  2, 8, 0,
       0, 0, 0,  4, 1, 9,  0, 0, 5,
       0, 0, 0,  0, 8, 0,  0, 7, 9]-}

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
