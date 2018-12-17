module MapGen (genMap)
where

import System.Random
import Data.Array
import Array2D
import Map

import Location

type Cell = IntVec2
type Trace = Path
type MoveOptions = Path

type MapGen = (Map, Array2D Visit)

genMap :: (Int, Int) -> Integer -> Map
-- generate a random Map
genMap size seed =
  fst.fst.walk maze [start] $gen
    where
      marr = makeMap size
      varr = makeArray2D size Unvisited
      (start, gen) = randomPos marr (mkStdGen (fromInteger seed))
      maze = (mark marr start Room, mark varr start Visited)

randomPos :: (RandomGen g) => Map -> g -> (Cell, g)
randomPos m gen =
  ((x, y), ygen)
    where
      (lower, range) = bounds.arr $m
      (x, xgen) = randomR (fst lower, fst range) gen
      (y, ygen) = randomR (snd lower, snd range) xgen


walk :: RandomGen g => MapGen -> Trace -> g -> (MapGen, Trace)
-- calculate one generation step
walk maze [] _ = (maze, [])
walk maze (pos:trace) gen = walk newmaze newtrace newgen
    where
      (newmaze, newtrace) = makeMove maze (pos:trace) mvs gen
      mvs = options maze pos -- options to move to
      (_, newgen) = next gen

makeMove :: (RandomGen g) => MapGen -> Trace -> MoveOptions -> g -> (MapGen, Trace)
-- determine the next generation step
makeMove maze (pos:trace) [] _ = (maze, trace) -- backtrace
makeMove maze (pos:trace) ops gen = (visitCell maze dst, dst:pos:trace)
    where
      (mx,my) = (selectRandom ops gen)
      (px, py) = pos
      dst = (px+mx,py+my)


visitCell :: MapGen -> Cell -> MapGen
-- move the generation head from A to B in a Map
visitCell (marr, varr) dst = (markRoom marr dst, visit varr dst)



options :: MapGen -> Cell -> MoveOptions
-- visitable neighbors of an input Cell in a Map
options (marr, varr) pos = [move | move <- moves, isSuitable move]
    where
      (_, (sx, sy)) = bounds.arr $marr
      moves = [(1, 0), (0,1), ((-1), 0), (0,(-1))]
      isSuitable move = inBounds marr dst &&
                        isAt dst marr Wall && not (nextToVisited dst)
                            where
                              (mx, my) = move
                              (px, py) = pos
                              dst =(px+mx, py+my)
      nextToVisited (px,py) = 3 < length (neighborsWith varr (px,py) (==Visited))

selectRandom :: RandomGen g => [a] -> g -> a
-- select a random element from a list using a RandomGen
selectRandom list gen = list!!(fst (randomR (0,(length list)-1) gen))
