module World
where

import Map
import Player
import Render
import Location
import MazeGen
import Direction
import Item

import Data.Array
import Array2D
import System.Random

data World = World (Map Location) (Map Item) Player

instance Show World where
  show (World (Map lm) (Map im) p) = show canvas
    where
      canvas = (render (render (makeScreen lm) im) player)
      player = Array2D (listArray (pos p, pos p) [pixel p])


move :: World -> Direction -> World
move w@(World m im (Player (px,py))) dir
  | isPassable w dst = World m im (Player dst)
  | otherwise = w
    where
      (vx,vy) = dir2Vec dir
      dst = (px+vx, py-vy)

isPassable :: World -> IntVec2 -> Bool
isPassable (World (Map marr) _ _) pos = isAt marr pos Room

genWorld :: IntVec2 -> StdGen -> World
genWorld size seed = World maze im (Player start)
  where
    (maze, start) = genMaze size seed
    im = placeItems maze seed
