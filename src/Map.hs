module Map where

import Data.Word
import System.IO
import Data.Array
import Array2D

data Map a = Map {arr2D::Array2D a}

makeMap :: IntVec2 -> a -> (Map a)
makeMap (sx, sy) a =
  Map (makeArray2D (sx, sy) a)

data Border = Border

instance Show Border where
  show b = "#"

instance (Show a) => Show (Map a) where
  show (Map marr) = lines
      where
        (_, (sx, sy)) = bounds.arr $marr
        hborder = (concat.take (sx+1+2) $(cycle [show Border])) ++ "\n"
        lines = withBorders (show marr)
        withBorders str = hborder ++ show Border ++ vborder str ++ hborder
        vborder [] = []
        vborder ('\n':[]) = show Border ++ "\n"
        vborder ('\n':cs) = show Border ++ "\n" ++ show Border ++ vborder cs
        vborder (c:cs) = c:vborder cs

