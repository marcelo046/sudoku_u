import Data.List
import System.IO
import Grid

main = jogo(grid)

jogo oldGrid = do printarGrid oldGrid
                  putStrLn "digite a coordenada x: "
                  x <- lerNumero
                  putStrLn "digite a coordenada y: "
                  y <- lerNumero
                  putStrLn "digite o valor: "
                  num <- lerNumero
                  let newGrid = montarGrid x y num oldGrid
                  if (oldGrid == newGrid) then putStrLn "valor invalido, fora do limite ou valor repitido\n" else putStrLn ""
                  if venceu newGrid then putStrLn "venceu!!!" else jogo newGrid
