module GameOfLife.Forms
       ( block
       , place
       , Form
       , size
       ) where

import GameOfLife.Game

type Form = [(Coord)]

block :: Form
block = [(x,y) | x <- [0,1], y <- [0,1]]

place :: Board -> Coord -> Form -> [(Coord,State)]
place board offset form =
  let coordsInPlace = map (move offset) form
  in zip coordsInPlace (repeat Alive)

size :: Form -> Coord
size form = ((maximum $ map fst form) + 1,(maximum $ map snd form) + 1)
