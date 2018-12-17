module Location where

import Array2D

data Location = Wall | Room | Path deriving (Eq)
data Visit = Visited | Unvisited deriving (Eq)

instance Show Location  where
  show (Wall) = "#"
  show (Room) = " "
  show (Path) = "."

instance Show Visit  where
  show (Visited) = " "
  show (Unvisited) = "*"

visitPath :: (Array2D Visit) -> Path -> (Array2D Visit)
visitPath varr2D path = markList varr2D path (cycle [Visited])

visit :: (Array2D Visit) -> IntVec2 -> (Array2D Visit)
visit varr2D pos = mark varr2D pos Visited

markPath :: (Array2D Location) -> Path -> (Array2D Location)
markPath warr2D path = markList warr2D path (cycle [Path])

markRoom :: (Array2D Location) -> IntVec2 -> (Array2D Location)
markRoom warr2D pos = mark warr2D pos Room
