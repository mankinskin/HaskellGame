module World
where

import Map
import Player
import Render
import Location
import MazeGen


import Data.Array
import Array2D

data World = World (Map Location) Player

instance Show World where
  show (World (Map marr) p) = show (render (makeScreen marr) player)
    where player = Array2D (listArray (pos p, pos p) [pixel p])


genWorld :: IntVec2 -> Integer -> World
genWorld size seed = World maze (Player start)
  where
    (maze, start) = genMaze size seed
