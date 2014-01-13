module GameOfLife
       ( State(..)
       , Board
       , evolve
       , initBoard
       , setStates
       , nextGen ) where
import Data.List
import Data.Maybe
import qualified Data.Map as Map


data State = Alive | Dead deriving (Eq, Show)
type Coord = (Int,Int)
data Cell = Cell { cellState :: State
                 , cellCoord :: Coord } deriving Show
data Board = Board { boardGrid :: (Map.Map Coord Cell)
                   , boardWidth :: Int
                   , boardHeight :: Int}


initBoard :: Int -> Int -> Board
initBoard width height =
  let grid = Map.fromList $ [(c, Cell Dead c) | x <- [0..width - 1], y <- [0..height - 1], let c = (x,y)]
  in Board grid width height


setState :: Board -> State -> Coord -> Board
setState (Board grid width height) state (x,y)
  | y >= height || y < 0 = error "Height is off bounds"
  | x >= width || x < 0 = error "Width is off bounds"
  | otherwise =
    let c = (x,y)
        newGrid = Map.insert c (Cell state c) grid
    in Board newGrid width height


setStates :: Board -> State -> [Coord] -> Board
setStates board state = foldl (\board coord -> setState board state coord) board


neighbours :: Board -> Coord -> [Cell]
neighbours (Board grid width height) c@(x,y)
  | not (inBounds c) = error "Coordinate off bounds"
  | otherwise =
    let neighboursCoords = filter (/= c) $ filter inBounds [(x',y') | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1]]
    in map getCell neighboursCoords
  where
    inBounds (x,y) = x >= 0 && y >= 0 && x < width && y < height
    getCell (x,y) = fromJust $ Map.lookup (x,y) grid


nextGen :: Board -> Board
nextGen board =
  let
    livingNeighbours c = length $ filter (==Alive) $ map cellState (neighbours board c)
    takeState state = map cellCoord $ filter (\c -> cellState c == state) $ Map.elems $ boardGrid board
    underPop = filter (\coords -> (livingNeighbours coords) < 2) $ takeState Alive
    overPop = filter (\coords -> (livingNeighbours coords) > 3) $ takeState Alive
    newBorn = filter (\coords -> (livingNeighbours coords) == 3) $ takeState Dead
    revive b = setStates b Alive newBorn
    kill b = setStates b Dead (overPop ++ underPop)
  in kill $ revive board


evolve :: Board -> [Board]
evolve board =
  let next = nextGen board
  in next:evolve next


-- Show instances --

instance Show Board where
  show (Board grid width height) =
    intercalate "\n" $ map gridLine [0..height - 1]
    where gridLine l =
            concat $ map (charState . cellState . fromJust) [Map.lookup (x,l) grid | x <- [0..width -1]]
          charState state
            | state == Dead = " "
            | state == Alive = "@"
