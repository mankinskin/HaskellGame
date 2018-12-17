module Location where

import Array2D

data Location = Unpassable | Room
data Wall = Wall | Path
data Visit = Visited | Unvisited deriving (Eq)

instance Show Location  where
  show (Unpassable) = "//"
  show (Room) = " "

instance Show Wall  where
  show (Wall) = "#"
  show (Path) = " "

instance Show Visit  where
  show (Visited) = " "
  show (Unvisited) = "*"

visitPath :: (Array2D Visit) -> Path -> (Array2D Visit)
visitPath varr2D path = markList varr2D path (cycle [Visited])

visit :: (Array2D Visit) -> IntVec2 -> (Array2D Visit)
visit varr2D pos = mark varr2D pos Visited

breakWalls :: (Array2D Wall) -> Path -> (Array2D Wall)
breakWalls warr2D path = markList warr2D path (cycle [Path])

breakWall :: (Array2D Wall) -> IntVec2 -> (Array2D Wall)
breakWall warr2D pos = mark warr2D pos Path
