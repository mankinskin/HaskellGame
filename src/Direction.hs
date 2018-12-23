module Direction where

import Array2D
import Input

data Direction = North | East | South | West deriving Show

northCmd :: Input
northCmd = 'w'

eastCmd :: Input
eastCmd = 'd'

southCmd :: Input
southCmd = 's'

westCmd :: Input
westCmd = 'a'

moveCmds :: [Input]
moveCmds = [northCmd,eastCmd,southCmd,westCmd]

dir2Vec :: Direction -> IntVec2
dir2Vec North = (0,1)
dir2Vec East = (1,0)
dir2Vec South = (0,(-1))
dir2Vec West = ((-1),0)
