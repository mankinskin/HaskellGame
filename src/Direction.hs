module Direction where

import Array2D

data Direction = North | East | South | West deriving Show

--northCmds :: [String]
--northCmds = ["North", "north", "Up","up"]
--
--eastCmds :: [String]
--eastCmds = ["East", "east", "Right","right"]
--
--southCmds :: [String]
--southCmds = ["South", "south", "Down","down"]
--
--westCmds :: [String]
--westCmds = ["West", "west", "Left", "left"]

moveCmds :: [String]
moveCmds = ["North","East","South","West"]

dir2Vec :: Direction -> IntVec2
dir2Vec North = (0,1)
dir2Vec East = (1,0)
dir2Vec South = (0,(-1))
dir2Vec West = ((-1),0)
