module Maze where

import Data.Word
import System.Random
import Data.Array
import System.IO

import Map
import Location

data Maze = Maze {wallmap::(Map Wall), visitmap::(Map Visit)}

instance Show Maze where
  show (Maze (Map walls) (Map visits)) =
    lines
      where
        (_, (sx, sy)) = bounds visits
        (_, (wsx, wsy)) = bounds walls

        lines = mapline sx sy

        -- double recursion, baby
        -- append shown Walls and Visit values of Maze
        -- draw 3x3 Maze like this:
        -- *#*#*
        -- #####
        -- *#*#*
        -- #####
        -- *#*#*
        -- iterate over yi (from top to bottom)
        -- for each yi, print a mapline (starting with *#*#...)
        -- and a wallline (all walls, with walls from the wallmap)
        x xi = sx-xi
        y yi = sy-yi
        visit xi yi = (x xi, y yi)
        hwall xi yi = (x xi, y yi*2)
        vwall xi yi = (x xi, y yi*2+1)

        mapline 0 yi =
          show (visits!visit 0 yi) ++"\n" ++ wallline sx yi
        mapline xi yi =
          show (visits!visit xi yi) ++ show (walls!hwall xi yi) ++
                            mapline (xi-1) yi

        wallline _ 0 =
          "\n"
        wallline 0 yi =
          show (walls!vwall 0 yi) ++ "\n" ++ mapline sx (yi-1)
        wallline xi yi =
          show (walls!vwall xi yi) ++ show Wall ++
                            wallline (xi-1) yi


makeMaze :: MapVec2 -> Maze
makeMaze (sx, sy) =
  Maze (makeMap (sx, sy*2) Wall) (makeMap (sx, sy) Unvisited)

type Cell = MapVec2
type Trace = Path
type MoveOptions = Path

genMaze :: (Int, Int) -> Int -> Maze
-- generate a random Maze
genMaze size seed =
  fst.walk maze [start] $gen
    where
      m = makeMaze size
      (start, gen) = randomPos m (mkStdGen seed)
      maze = Maze (wallmap m) (visit (visitmap m) start)

randomPos :: (RandomGen g) => Maze -> g -> (Cell, g)
randomPos maze gen =
  ((x, y), ygen)
    where
      range = snd.bounds.arr.visitmap $maze
      (x, xgen) = randomR (0, fst range) gen
      (y, ygen) = randomR (0, snd range) xgen


walk :: RandomGen g => Maze -> Trace -> g -> (Maze, Trace)
-- calculate one generation step
walk maze [] _ =
  (maze, [])
walk maze (pos:trace) gen =
  walk newmaze newtrace newgen
    where
      (newmaze, newtrace) = makeMove maze (pos:trace) mvs gen
      mvs = options maze pos -- options to move to
      (_, newgen) = next gen

makeMove :: (RandomGen g) => Maze -> Trace -> MoveOptions -> g -> (Maze, Trace)
-- determine the next generation step
makeMove maze (pos:trace) [] _ =
  (maze, trace) -- backtrace
makeMove maze (pos:trace) ops gen =
  (moveHead maze pos dst, dst:pos:trace)
    where (mx,my) = (selectRandom ops gen)
          (px, py) = pos
          dst = (px+mx,py+my)




moveHead :: Maze -> Cell -> Cell -> Maze
-- move the generation head from A to B in a Maze
moveHead maze pos dst =
  Maze  (breakWall (wallmap maze) (wallx, wally))
        (visit (visitmap maze) dst)
    where wallx = min (fst dst) (fst pos)
          wally
            | snd pos == snd dst = snd dst * 2
            | otherwise = min (snd pos) (snd dst) * 2 + 1


options :: Maze -> Cell -> MoveOptions
-- visitable neighbors of an input Cell in a Maze
options maze (px, py) =
  [(bx,by) | (bx,by) <- moves, isSuitable (px+bx,py+by)]
    where
      (_, (sx, sy)) = bounds.arr.visitmap $maze
      moves = [(1, 0), (0,1), ((-1), 0), (0,(-1))]
      inBounds (x, y) = (0 <= x) && (x <= sx) && (0 <= y) && (y <= sy)
      isUnvisited pos = (arr.visitmap $maze)!pos == Unvisited
      isSuitable pos = inBounds pos && isUnvisited pos

selectRandom :: RandomGen g => [a] -> g -> a
-- select a random element from a list using a RandomGen
selectRandom list gen =
  list!!(fst (randomR (0,(length list)-1) gen))



