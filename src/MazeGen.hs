module MazeGen (genMaze)
where

import System.Random
import Data.Array

import Array2D
import Map
import Location

type Tile = IntVec2
type Trace = Path
type MoveOptions = Path

-- MazeGen, used during Maze generation
-- Stores resulting Map and an Array2D
-- keeping track of the tiles visited
type Maze = Map Location
type MazeGen = (Maze, Array2D Visit)

-- generate a random Maze Map
-- uses a simple depth first search algorithm:

-- start with a Map completely filled with Walls
-- and all tiles marked as 'Unvisited'
-- pick random starting position
-- mark start pos Visited
-- and remove Wall (set to 'Room')
-- find all possible move directions
-- move in a random direction
-- push former position on a Trace stack
-- repeat until no move is possible
-- walk the trace back until moves are possible
-- repeat until back at starting position

genMaze :: (Int, Int) -> StdGen -> (Maze, IntVec2)
-- start Maze generation using a size and a RNG seed
-- returns a random Maze and the starting position
genMaze size seed = (mmap, start)
    where
      (Map marr) = makeMap size Wall
      varr = makeArray2D size Unvisited
      (start, gen) = randomPos marr seed
      mazegen = (Map (mark marr start Room), mark varr start Visited)
      ((mmap, _),_) = genstep (mazegen, [start]) gen


genstep :: (MazeGen, Trace) -> StdGen -> (MazeGen, Trace)
-- recursive generation step
-- finds possible moves and calls makeMove
genstep (maze, []) _ = (maze, []) -- trace empty, stop generation
genstep (maze, (pos:trace)) gen = genstep newmaze newgen
    where
      newmaze = makeMove (maze,(pos:trace)) ops gen
      ops = options maze pos -- options to move to
      (_, newgen) = next gen

makeMove :: (MazeGen, Trace) -> MoveOptions -> StdGen -> (MazeGen, Trace)
-- decides whether to backtrace if no tile is suitable
-- or go to the next suitable tile
makeMove (maze, (pos:trace)) [] _ = (maze, trace) -- backtrace
makeMove (maze ,(pos:trace)) ops gen = visitTile (maze, (pos:trace)) dst
    where
      (mx,my) = (selectRandom ops gen)
      (px, py) = pos
      dst = (px+mx,py+my)

visitTile :: (MazeGen, Trace) -> Tile -> (MazeGen, Trace)
-- move to tile in Map, mark as Visited and as Room
visitTile (((Map marr), varr), trace) dst = (newmaze, dst:trace)
  where newmaze = (Map (markRoom marr dst), visit varr dst)

options :: MazeGen -> Tile -> MoveOptions
-- visitable neighbors of an input Tile in a Map
options (Map marr, varr) pos = [move | move <- moves, isSuitable move]
    where
      (_, (sx, sy)) = bounds.arr $marr
      moves = [(1, 0), (0,1), ((-1), 0), (0,(-1))]
      isSuitable move = isAt marr dst Wall &&
                        nearUnvisited dst
                            where
                              (mx, my) = move
                              (px, py) = pos
                              dst =(px+mx, py+my)
      nearUnvisited (px,py) =
          -- each tile has a max of 8 neighbors
          -- one is always already visited (the current position)
          -- tile can have max 2 neighbors visited to be suitable
          -- eg 1 in addition to the current tile (trace head)
          length (neighborsWith varr (px,py) (==Visited)) < 3

selectRandom :: [a] -> StdGen -> a
-- select a random element from a list using a RandomGen
selectRandom list gen = list!!(fst (randomR (0,(length list)-1) gen))

