module Map where

import Data.Word
import System.IO
import Data.Array
import Array2D

import Location

type Map = Array2D Location

makeMap :: IntVec2 -> Map
makeMap (sx, sy) =
  makeArray2D (sx, sy) Wall


--instance Show Map where
--  show map =
--    lines
--      where
--        (_, (sx, sy)) = bounds map
--
--        lines = topborder ++"\n"++ show Wall ++ line sx sy
--          where topborder = concat.take ((sx+1)*2+1) $(cycle [show $Wall])
--
--        at xi yi = (sx-xi, sy-yi)
--
--        line xi yi =
--           show (map!at xi yi) ++ next xi
--            where
--                  next 0 = "\n"++show Wall ++line sx (yi-1)
--                  next nxi = mapline (nxi-1) yi
--
--        --wallline xi yi =
--        --  wall ++ show Wall ++ next xi yi
--        --    where
--        --          wall = show (walls!vwall xi yi)
--        --          next 0 0 = "\n"
--        --          next nxi 0 = wallline (nxi-1) 0
--        --          next 0 nyi = "\n"++show Wall++mapline sx (nyi-1)
--        --          next nxi nyi= wallline (nxi-1) nyi
--
