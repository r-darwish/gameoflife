module GameOfLife.Game
       ( State(..)
       , Board
       , Coord
       , move
       , evolve
       , initBoard
       , setStates
       , nextGen
       ) where
import Data.List
import Data.Array
import System.IO
import Control.Monad

data State = Alive | Dead deriving (Eq, Show)
type Coord = (Int,Int)
type Board = Array Coord State

initBoard :: Coord -> Board
initBoard (width,height) =
  let bounds = ((0,0),(width - 1,height - 1))
  in array bounds $ zip (range bounds) (repeat Dead)

setStates :: Board -> [(Coord,State)] -> Board
setStates = (//)

getStates :: Board -> [Coord] -> [State]
getStates board coords = map (board!) coords

neighbours :: Board -> Coord -> [Coord]
neighbours board c@(dx,dy) = do
  x <- [dx - 1..dx + 1]
  y <- [dy - 1..dy + 1]
  let point = (x,y)
  guard $ point /= c
  guard $ inRange (bounds board) point
  return point

nextGen :: Board -> Board
nextGen board =
  let
    allCells = range (bounds board)
    takeState dstate coords = do
      coord <- coords
      let state = board ! coord
      guard $ state == dstate
      return (coord)
    theLiving = takeState Alive allCells
    theDead = takeState Dead allCells
    livingNeighbours = length . takeState Alive . neighbours board
    underPop = do
      alive <- theLiving
      guard $ (livingNeighbours alive) < 2
      return (alive,Dead)
    overPop = do
      alive <- theLiving
      guard $ (livingNeighbours alive) > 3
      return (alive,Dead)
    newBorn = do
      dead <- theDead
      guard $ (livingNeighbours dead) == 3
      return (dead,Alive)
  in setStates board (concat [underPop, overPop, newBorn])

evolve :: Board -> [Board]
evolve = iterate nextGen

move :: Coord -> Coord -> Coord
move (ox,oy) (x,y) = (ox+x,oy+y)
