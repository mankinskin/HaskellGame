module Player
where

import Array2D
import Render

data Player = Player {pos::IntVec2}

instance Show Player where
  show p = "@"

instance PixelType Player where
  pixel p = Pixel '@'
