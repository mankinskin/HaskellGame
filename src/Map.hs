module Map where

import Data.Word
import System.IO
import Data.Array
import Array2D

import Location

data Map = Map (Array2D Location)

makeMap :: IntVec2 -> Map
makeMap (sx, sy) =
  Map (makeArray2D (sx, sy) Wall)


instance Show Map where
  show (Map (Array2D marr)) = lines
      where
        (_, (sx, sy)) = bounds marr

        hborder = (concat.take (sx+1+2) $(cycle [show $Wall])) ++"\n"
        lines = foldr (++) hborder (hborder:map line [sy,sy-1..0])

        at xi yi = show (marr!(sx-xi, sy-yi))

        line yi = show Wall ++ tile sx yi ++ show Wall ++ "\n"

        tile 0 yi= at 0 yi
        tile xi yi = at xi yi ++ tile (xi-1) yi

