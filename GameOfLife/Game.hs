module GameOfLife.Game
       ( State(..)
       , Board
       , Coord
       , move
       , evolve
       , initBoard
       , setStates
       , nextGen
       , toText
       ) where
import Data.List
import Data.Array
import System.IO
import qualified Data.Text as T


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
neighbours board c@(x,y) =
  filter (/= c) $ filter (inRange (bounds board)) [(x',y') | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1]]


nextGen :: Board -> Board
nextGen board =
  let
    allCells = range (bounds board)
    takeState state coords = map fst . filter (\(_,s) -> s == state) $ zip coords (getStates board coords)
    livingNeighbours = length . takeState Alive . neighbours board
    zipState state coords = zip coords (repeat state)
    underPop = zipState Dead . filter (\c -> (livingNeighbours c) < 2) $ takeState Alive allCells
    overPop = zipState Dead .filter (\c -> (livingNeighbours c) > 3) $ takeState Alive allCells
    newBorn = zipState Alive .filter (\c -> (livingNeighbours c) == 3) $ takeState Dead allCells
  in setStates board (concat [underPop, overPop, newBorn])


evolve :: Board -> [Board]
evolve = iterate nextGen


move :: Coord -> Coord -> Coord
move (ox,oy) (x,y) = (ox+x,oy+y)


toText :: Board -> T.Text
toText board = T.intercalate (T.singleton '\n') (rows minY)
  where
    ((minX,minY),(maxX,maxY)) = bounds board
    rows y
      | y > maxY = []
      | otherwise = (row y minX):rows (y + 1)
    row y x
      | x > maxX = T.empty
      | otherwise = T.cons (stateToChar $ board!(x,y)) (row y (x + 1))
    stateToChar state
      | state == Alive = '@'
      | otherwise = ' '
