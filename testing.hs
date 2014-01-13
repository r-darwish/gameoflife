import System.IO
import System.Random
import GameOfLife
import System.Console.ANSI
import Control.Concurrent


x wSeed hSeed =
  let
    height = 60
    width = 160
    board = initBoard width height
    coords = zip (randomize wSeed width 2000) (randomize hSeed height 2000)
      where randomize seed upper n = (take n $ randomRs (0, upper - 1) (mkStdGen seed))
  in setStates board Alive coords


loop :: Board -> IO Board
loop b = do
  clearScreen
  setCursorPosition 0 0
  putStrLn (show b)
  threadDelay 100000
  loop $ nextGen b


main = do
  loop (x 1523 1244)
