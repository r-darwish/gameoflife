module GameOfLife.Forms
       ( block
       , boat
       , toad
       , beacon
       , pulsar
       , blinker
       , beehive
       , loaf
       , place
       , Form
       , size
       ) where

import GameOfLife.Game

type Form = [(Coord)]

block :: Form
block = [(x,y) | x <- [0,1], y <- [0,1]]

beehive :: Form
beehive = [ (1,0),(2,0)
          , (0,1),(3,1)
          , (1,2),(2,2) ]

loaf :: Form
loaf = [ (1,0),(2,0)
       , (0,1),(3,1)
       , (1,2),(3,2)
       , (2,3) ]

boat :: Form
boat = [ (0,0),(1,0)
       , (0,1),(2,1)
       , (1,2) ]

blinker :: Form
blinker = [(x,1) | x <- [0..2]]

toad :: Form
toad = [ (2,0)
       , (0,1),(3,1)
       , (0,2),(3,2)
       , (1,3) ]

beacon :: Form
beacon = [ (0,0),(1,0)
         , (0,1)
         , (3,2)
         , (2,3),(3,3) ]

pulsar :: Form
pulsar = [ (2,0),(3,0),(4,0),(8,0),(9,0),(10,0)
         , (0,2),(5,2),(7,2),(12,2)
         , (0,3),(5,3),(7,3),(12,3)
         , (0,4),(5,4),(7,4),(12,4)
         , (2,5),(3,5),(4,5),(8,5),(9,5),(10,5)
         , (2,7),(3,7),(4,7),(8,7),(9,7),(10,7)
         , (0,8),(5,8),(7,8),(12,8)
         , (0,9),(5,9),(7,9),(12,9)
         , (0,10),(5,10),(7,10),(12,10)
         , (2,12),(3,12),(4,12),(8,12),(9,12),(10,12)
         ]

place :: Board -> Coord -> Form -> [(Coord,State)]
place board offset form =
  let coordsInPlace = map (move offset) form
  in zip coordsInPlace (repeat Alive)

size :: Form -> Coord
size form = ((maximum $ map fst form) + 1,(maximum $ map snd form) + 1)
