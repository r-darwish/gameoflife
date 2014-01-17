module GameOfLife.Forms
       ( Block(..)
       , place
       , Form
       , toCoords
       , size
       ) where

import GameOfLife.Game

class Form a where
  toCoords :: a -> [(Coord)]
  size :: a -> (Coord)

data Block = Block
instance Form Block where
  toCoords _ = [(x,y) | x <- [0,1], y <- [0,1]]
  size _ = (2,2)

place :: Form a => Board -> Coord -> a -> [(Coord,State)]
place board offset form =
  let coordsInPlace = map (move offset) (toCoords form)
  in zip coordsInPlace (repeat Alive)
