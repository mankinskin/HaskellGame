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

genMap :: (Int, Int) -> Integer -> Map
-- generate a random Map
genMap size seed =
  fst.walk maze [start] $gen
    where
      m = makeMap size
      (start, gen) = randomPos m (mkStdGen (fromInteger seed))
      maze = Map (wallarr m) (visit (visitarr m) start)

randomPos :: (RandomGen g) => Map -> g -> (Cell, g)
randomPos maze gen =
  ((x, y), ygen)
    where
      range = snd.bounds.arr.visitarr $maze
      (x, xgen) = randomR (0, fst range) gen
      (y, ygen) = randomR (0, snd range) xgen


walk :: RandomGen g => Map -> Trace -> g -> (Map, Trace)
-- calculate one generation step
walk maze [] _ =
  (maze, [])
walk maze (pos:trace) gen =
  walk newmaze newtrace newgen
    where
      (newmaze, newtrace) = makeMove maze (pos:trace) mvs gen
      mvs = options maze pos -- options to move to
      (_, newgen) = next gen

makeMove :: (RandomGen g) => Map -> Trace -> MoveOptions -> g -> (Map, Trace)
-- determine the next generation step
makeMove maze (pos:trace) [] _ =
  (maze, trace) -- backtrace
makeMove maze (pos:trace) ops gen =
  (moveHead maze pos dst, dst:pos:trace)
    where (mx,my) = (selectRandom ops gen)
          (px, py) = pos
          dst = (px+mx,py+my)


moveHead :: Map -> Cell -> Cell -> Map
-- move the generation head from A to B in a Map
moveHead (Map warr varr) pos dst =
  Map (breakWall warr (wallx, wally))
      (visit varr dst)
    where wallx = min (fst dst) (fst pos)
          wally
            | snd pos == snd dst = snd dst * 2
            | otherwise = min (snd pos) (snd dst) * 2 + 1


options :: Map -> Cell -> MoveOptions
-- visitable neighbors of an input Cell in a Map
options maze (px, py) =
  [(bx,by) | (bx,by) <- moves, isSuitable (px+bx,py+by)]
    where
      (_, (sx, sy)) = bounds.arr.visitarr $maze
      moves = [(1, 0), (0,1), ((-1), 0), (0,(-1))]
      inBounds (x, y) = (0 <= x) && (x <= sx) && (0 <= y) && (y <= sy)
      isUnvisited pos = (arr.visitarr $maze)!pos == Unvisited
      isSuitable pos = inBounds pos && isUnvisited pos

selectRandom :: RandomGen g => [a] -> g -> a
-- select a random element from a list using a RandomGen
selectRandom list gen =
  list!!(fst (randomR (0,(length list)-1) gen))
