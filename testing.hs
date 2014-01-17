import System.IO
import System.Random
import System.Console.ANSI
import Control.Concurrent
import qualified Data.Text.IO as TIO
import GameOfLife.Game
import GameOfLife.Forms
import GameOfLife.Text

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

main = let
  boardWidth = 150
  boardHeight = 70
  board = initBoard (boardWidth,boardHeight)
  form = pulsar
  space = 10
  (formWidth,formHeight) = size form
  blocks = concat [place board (x,y) form
                  | x <- [2,(formWidth + space)..(boardWidth - formWidth)]
                  , y <- [2,(formHeight + space)..(boardHeight - formHeight)]]
  in do loop $ setStates board blocks
