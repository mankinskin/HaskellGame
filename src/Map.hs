module Map where

import Data.Array

type MapVec2 = (Int, Int) -- Vec in a Map
type Path = [MapVec2] -- List of Vecs

-- 2D Map
data Map a = Map {arr::Array MapVec2 a}

instance (Show a) => Show (Map a) where
  show (Map arr) = lines sy
    where
      (sx, sy) = snd.bounds $arr
      lines 0 = line sy sx
      lines yi = line (sy-yi) sx++lines (yi-1)
      line yi 0 = show (arr!(sx-0, yi)) ++ "\n"
      line yi xi = show (arr!(sx-xi, yi)) ++ (line yi (xi-1))


makeMap :: MapVec2 -> a -> (Map a)
-- Creates a Map of a given size and with given initial value
makeMap (sx, sy) zro = Map (listArray ((0, 0), (sx-1, sy-1)) arr)
 where arr = take ((sx)*(sy)).cycle $[zro]


markPath :: (Map a) -> Path -> [a] -> (Map a)
-- sets values of given positions in Map
markPath (Map arr) ps vals = Map (arr//updates)
  where updates = (zip ps vals) --updated assocs

markPos :: (Map a) -> MapVec2 -> a -> (Map a)
-- sets values of given positions in Map
markPos map p val = markPath map [p] [val]
