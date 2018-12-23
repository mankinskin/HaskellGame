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

printInventory :: Player -> IO()
printInventory (Player _ []) = putStrLn "Your inventory is empty."
printInventory (Player _ inv) = putStr ("You have:\n"++entries inv)
                where
                  entries [] = ""
                  entries (e:es) = entry e ++ "\n" ++ entries es
                  entry (i,cnt) = show cnt ++ "x "++ show i
