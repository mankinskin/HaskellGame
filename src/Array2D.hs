module Array2D where

import Data.Array

type IntVec2 = (Int, Int) -- Vec in a Array2D
type Path = [IntVec2] -- List of Vecs

-- 2D array
data Array2D a = Array2D {arr::Array IntVec2 a}

instance (Show a) => Show (Array2D a) where
  show (Array2D arr) = lines sy
    where
      (sx, sy) = snd.bounds $arr
      lines 0 = line sy sx
      lines yi = line (sy-yi) sx++lines (yi-1)
      line yi 0 = show (arr!(sx-0, yi)) ++ "\n"
      line yi xi = show (arr!(sx-xi, yi)) ++ (line yi (xi-1))


makeArray2D :: IntVec2 -> a -> (Array2D a)
-- Creates a Array2D of a given size and with given initial value
makeArray2D (sx, sy) zro = Array2D (listArray ((0, 0), (sx-1, sy-1)) arr)
 where arr = take ((sx)*(sy)).cycle $[zro]


markList :: (Array2D a) -> Path -> [a] -> (Array2D a)
-- sets values of given positions in Array2D
markList (Array2D arr) ps vals = Array2D (arr//updates)
  where updates = (zip ps vals) --updated assocs

mark :: (Array2D a) -> IntVec2 -> a -> (Array2D a)
-- sets values of given positions in Array2D
mark arr2D p val = markList arr2D [p] [val]
