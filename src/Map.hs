module Map where

import Data.Array

type MapVec2 = (Int, Int) -- Vec in a Map
type Path = [MapVec2] -- List of Vecs

-- 2D Map
data Map a = Map (Array MapVec2 a)

instance (Show a) => Show (Map a) where
  show (Map arr) = foldr (++) "" (lines sy)
    where
      (sx, sy) = snd.bounds $arr
      lines 0 = []
      lines yi = (foldr (++) "\n" (line (sy-yi) sx)):lines (yi-1)
      line _ 0 = []
      line yi xi = show (arr!(sx-xi, yi)):(line yi (xi-1))


makeMap :: MapVec2 -> a -> (Map a)
-- Creates a Map of a given size and with given initial value
makeMap (sx, sy) zro = Map (listArray ((0, 0), (sx, sy)) arr)
 where arr = take ((sx+1)*(sy+1)).cycle $[zro]


markPath :: (Map a) -> Path -> [a] -> (Map a)
-- sets values of given positions in Map
markPath (Map arr) ps vals = Map (arr//updates)
  where updates = (zip ps vals) --updated assocs

markPos :: (Map a) -> MapVec2 -> a -> (Map a)
-- sets values of given positions in Map
markPos map p val = markPath map [p] [val]
