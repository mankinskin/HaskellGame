module Direction where

import Array2D

data Direction = North | East | South | West deriving Show

dirStrings :: [String]
dirStrings = ["North","East","South","West"]

dir2Vec :: Direction -> IntVec2
dir2Vec North = (0,(-1))
dir2Vec East = (1,0)
dir2Vec South = (0,1)
dir2Vec West = ((-1),0)
