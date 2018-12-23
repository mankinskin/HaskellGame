module World
where

import Map
import Player
import Render
import Location
import MazeGen
import Direction
import Item
import Array2D

import Data.Array
import Array2D
import System.Random

data World = World {lmap::Map Location, imap::Map Item, player::Player}

instance Show World where
  show (World (Map lm) (Map im) p) = show canvas
    where
      canvas = (render (render (makeScreen lm) im) pimg)
      pimg = Array2D (listArray (pos p, pos p) [pixel p])


move :: World -> Direction -> World
move w@(World m im (Player (px,py) inv)) dir
  | isPassable w dst = World m im (Player dst inv)
  | otherwise = w
    where
      (vx,vy) = dir2Vec dir
      dst = (px+vx, py-vy)

isPassable :: World -> IntVec2 -> Bool
isPassable (World (Map marr) _ _) pos = isAt marr pos Room

genWorld :: IntVec2 -> StdGen -> World
genWorld size seed = World maze im (Player start [])
  where
    (maze, start) = genMaze size seed
    im = placeItems maze seed

interact :: World -> World
interact w@(World lm m@(Map im) (Player pos inv))
      | isAt im pos None = w
      | otherwise = World (lmap w) nim (Player pos ninv)
        where (nim, ninv) = pickupItem m pos inv
