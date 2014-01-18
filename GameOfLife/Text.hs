module GameOfLife.Text
       ( toText
       ) where

import Data.Array
import qualified Data.Text as T
import GameOfLife.Game

toText :: Board -> T.Text
toText board = T.intercalate (T.singleton '\n') (map row [minY..maxY])
  where
    ((minX,minY),(maxX,maxY)) = bounds board
    row y =  foldr (\x acc -> T.cons (stateToChar $ board!(x,y)) acc) T.empty [minX..maxX]
    stateToChar state
      | state == Alive = '@'
      | otherwise = ' '
