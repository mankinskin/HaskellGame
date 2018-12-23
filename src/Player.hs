module Player
where

import Array2D
import Render
import Item

data Player = Player {pos::IntVec2, inventory::Inventory}

instance Show Player where
  show p = "@"

instance PixelType Player where
  pixel p = Pixel '@'
