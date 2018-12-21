module Location where

import Data.Array
import Array2D
import Render

data Location = Wall | Room deriving (Eq)
data Visit = Visited | Unvisited deriving (Eq)

instance PixelType Location where
  pixel Wall = Pixel '#'
  pixel Room = Pixel ' '

instance Show Visit  where
  show (Visited) = " "
  show (Unvisited) = "*"

instance Render Location where

visit :: (Array2D Visit) -> IntVec2 -> (Array2D Visit)
visit varr2D pos = mark varr2D pos Visited

markRoom :: (Array2D Location) -> IntVec2 -> (Array2D Location)
markRoom warr2D pos = mark warr2D pos Room
