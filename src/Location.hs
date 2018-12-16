module Location where

import Map

data Location = Unpassable | Room
data Wall = Wall | Path
data Visit = Visited | Unvisited deriving (Eq)

instance Show Location  where
  show (Unpassable) = "//"
  show (Room) = " "

instance Show Wall  where
  show (Wall) = "#"
  show (Path) = "."

instance Show Visit  where
  show (Visited) = " "
  show (Unvisited) = "*"

visitPath :: (Map Visit) -> Path -> (Map Visit)
visitPath vmap path = markPath vmap path (cycle [Visited])

visit :: (Map Visit) -> MapVec2 -> (Map Visit)
visit vmap pos = markPos vmap pos Visited

breakWalls :: (Map Wall) -> Path -> (Map Wall)
breakWalls wmap path = markPath wmap path (cycle [Path])

breakWall :: (Map Wall) -> MapVec2 -> (Map Wall)
breakWall wmap pos = markPos wmap pos Path
