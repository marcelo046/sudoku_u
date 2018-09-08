import Data.List
import System.IO
import Grid

main = jogo(grid)

jogo oldGrid = do printarGrid oldGrid
                  putStrLn "digite a linha: "
                  y <- lerNumero
                  putStrLn "digite a coluna: "
                  x <- lerNumero
                  putStrLn "digite o valor: "
                  num <- lerNumero
                  let newGrid = montarGrid (x-1) (y-1) num oldGrid
                  if (oldGrid == newGrid) then putStrLn "valor invalido, fora do limite ou valor repitido\n" else putStrLn ""
                  if venceu newGrid then putStrLn "\n\nVenceu!!!" else jogo newGrid
