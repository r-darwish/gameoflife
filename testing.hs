import System.IO
import System.Random
import GameOfLife
import System.Console.ANSI
import Control.Concurrent
import qualified Data.Text.IO as TIO


x wSeed hSeed =
  let
    height = 60
    width = 160
    board = initBoard (width,height)
    coords = zip (randomize wSeed width 1500) (randomize hSeed height 1500)
      where randomize seed upper n = (take n $ randomRs (0, upper - 1) (mkStdGen seed))
  in setStates board (zip coords $ repeat Alive)


loop :: Board -> IO Board
loop b = do
  clearScreen
  setCursorPosition 0 0
  TIO.putStrLn $ toText b
  threadDelay 100000
  loop $ nextGen b


main = do
  loop (x 1523 1244)
