module Map where

import Data.Word
import System.IO
import Data.Array
import Array2D

import Location

data Map = Map {wallarr::(Array2D Wall), visitarr::(Array2D Visit)}

makeMap :: IntVec2 -> Map
makeMap (sx, sy) =
  Map (makeArray2D (sx, sy*2) Wall) (makeArray2D (sx, sy) Unvisited)


instance Show Map where
  show (Map (Array2D walls) (Array2D visits)) =
    lines
      where
        (_, (sx, sy)) = bounds visits
        (_, (wsx, wsy)) = bounds walls

        lines = topborder ++"\n"++show Wall ++ mapline sx sy
          where topborder = concat.take ((sx+1)*2+1) $(cycle [show $Wall])

        -- double recursion, baby
        -- append shown Walls and Visit values of Map
        -- draw 3x3 Map like this:
        -- #######
        -- #*#*#*#
        -- #######
        -- #*#*#*#
        -- #######
        -- #*#*#*#
        -- #######
        -- iterate over yi (from top to bottom)
        -- for each yi, print a mapline (starting with *#*#...)
        -- and a wallline (all walls, with walls from the wallarr)
        x xi = sx-xi
        y yi = sy-yi
        visit xi yi = (x xi, y yi)
        hwall xi yi = (x xi, y yi*2)
        vwall xi yi = (x xi, y yi*2+1)

        mapline xi yi =
           tile ++ wall ++ next xi
            where
                  tile = show (visits!visit xi yi)
                  wall = show (walls!hwall xi yi)
                  next 0 = "\n"++show Wall ++wallline sx yi
                  next nxi = mapline (nxi-1) yi

        wallline xi yi =
          wall ++ show Wall ++ next xi yi
            where
                  wall = show (walls!vwall xi yi)
                  next 0 0 = "\n"
                  next nxi 0 = wallline (nxi-1) 0
                  next 0 nyi = "\n"++show Wall++mapline sx (nyi-1)
                  next nxi nyi= wallline (nxi-1) nyi

