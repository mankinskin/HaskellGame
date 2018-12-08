module Map where

import System.IO


data Location = Impassable | Room deriving Show

data Map = Map [[Location]]

instance Show Map where
  show (Map locations) = foldr (++) "" (map foldLine locations)
    where
      foldLine line = foldr (++) "\n" ((show.head $line):(map (((++) "\t") . show).init $line))
