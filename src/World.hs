module World
where

import Map
import Player
import Render
import Location
import MazeGen
import Direction


import Data.Array
import Array2D

data World = World (Map Location) Player

instance Show World where
  show (World (Map marr) p) = show (render (makeScreen marr) player)
    where player = Array2D (listArray (pos p, pos p) [pixel p])


move :: World -> Direction -> World
move w@(World m (Player (px,py))) dir
  | isPassable w dst = World m (Player dst)
  | otherwise = w
    where
      (vx,vy) = dir2Vec dir
      dst = (px+vx, py-vy)

isPassable :: World -> IntVec2 -> Bool
isPassable (World (Map marr) _) pos = isAt marr pos Room

genWorld :: IntVec2 -> Integer -> World
genWorld size seed = World maze (Player start)
  where
    (maze, start) = genMaze size seed
