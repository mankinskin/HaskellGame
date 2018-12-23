module Item
where

import Location
import Map
import Array2D
import Render

import System.Random
import Data.List
import Data.Array

data Item = None | Rock | Key deriving Eq

instance PixelType Item where
  pixel None = pixel Room
  pixel Rock = Pixel '.'
  pixel Key = Pixel '§'

instance Render Item where
  render dst src = Map . Array2D $(accum (\d s -> s) dsts srcs)
    where
      dsts = arr.arr2D $dst
      srcs = mksrcs.assocs.arr$src
      mksrcs [] = []
      mksrcs ((i, None):ias) = (i,dsts!i):mksrcs ias
      mksrcs ((i, a):ias) = (i,pixel a):mksrcs ias

type Inventory = [(Item, Int)]

putItemInInventory :: Item -> Inventory -> Inventory
putItemInInventory it inv = doToItemCount (+1) it inv

removeItemInInventory :: Item -> Inventory -> Inventory
removeItemInInventory it inv = doToItemCount ((-) 1) it inv

doToItemCount :: (Int -> Int) -> Item -> Inventory -> Inventory
doToItemCount f it [] = [(it, f 0)]
doToItemCount f it ((i,cnt):es)
  | i == it = (it, f cnt):es
  | otherwise = (i, cnt):doToItemCount f it es

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

placeItems :: (Map Location) -> StdGen -> (Map Item)
placeItems (Map mz) seed = Map . Array2D $itms
    where
      bnds = bounds.arr$mz
      lasscs = assocs.arr$mz
      itms = array bnds (plc seed lasscs)
      plc :: StdGen -> [(IntVec2, Location)] -> [(IntVec2, Item)]
      plc g [] = []
      plc g ((ix, Wall):ls) = (ix, None):plc g ls
      plc g ((ix, Room):ls) = (ix, randomItem i):plc ng ls
                      where (i, ng) = next g

randomItem :: Int -> Item
randomItem sd
  | i <= keyprob   = Key
  | keyprob < i && i <= rockprob = Rock
  | otherwise = None
    where
      i = mod sd totprob
      keyprob = 1
      rockprob = 39
      noneprob = 60
      totprob = keyprob + rockprob + noneprob

