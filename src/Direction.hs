module Direction where

import Array2D

data Direction = North | East | South | West deriving Show

northCmds :: [String]
northCmds = ["north", "up", "w"]

eastCmds :: [String]
eastCmds = ["east", "right", "d"]

southCmds :: [String]
southCmds = ["south", "down", "s"]

westCmds :: [String]
westCmds = ["west", "left", "a"]

moveCmds :: [String]
moveCmds = northCmds ++ eastCmds ++ southCmds ++ westCmds

dir2Vec :: Direction -> IntVec2
dir2Vec North = (0,1)
dir2Vec East = (1,0)
dir2Vec South = (0,(-1))
dir2Vec West = ((-1),0)
