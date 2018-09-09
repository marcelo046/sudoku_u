import Data.List
import System.IO
import Grid

main = do putStrLn "                    <==--[ SuDoKu ]--==>\n"
          putStrLn "                Preencha com valores de 1 a 9"
          putStrLn "    Ou 0 para apagar uma celula, ou -1 para resetar o jogo"
          putStrLn "Nao pode haver valores repitidos em uma linha, coluna ou bloco\n"
          jogo(grid)

jogo oldGrid = do printarGrid oldGrid
                  putStrLn "digite a linha: "
                  y <- lerNumero
                  putStrLn "digite a coluna: "
                  x <- lerNumero
                  putStrLn "digite o valor: "
                  num <- lerNumero
                  let newGrid = montarGrid (x-1) (y-1) num oldGrid
                  if (oldGrid == newGrid) then putStrLn "valor invalido, fora do limite ou valor repitido\n" else putStrLn ""
                  if venceu newGrid then do putStrLn "\n\nParabens! Voce venceu!!!"
                                            printarGrid newGrid
                  else jogo newGrid
