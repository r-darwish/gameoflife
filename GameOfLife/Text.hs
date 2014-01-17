module GameOfLife.Text
       ( toText
       ) where

import Data.Array
import qualified Data.Text as T
import GameOfLife.Game


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
