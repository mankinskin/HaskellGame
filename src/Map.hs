module Map where

import Data.Word
import System.Random
import Data.Array
import System.IO

import Array2D
import Location

data Map = Map {wallarr::(Array2D Wall), visitarr::(Array2D Visit)}

instance Show Map where
  show (Map (Array2D walls) (Array2D visits)) =
    lines
      where
        (_, (sx, sy)) = bounds visits
        (_, (wsx, wsy)) = bounds walls

        lines = mapline sx sy

        -- double recursion, baby
        -- append shown Walls and Visit values of Map
        -- draw 3x3 Map like this:
        -- #######
        -- #*#*#*#
        -- #######
        -- #*#*#*#
        -- #######
        -- #*#*#*#
        -- #######
        -- iterate over yi (from top to bottom)
        -- for each yi, print a mapline (starting with *#*#...)
        -- and a wallline (all walls, with walls from the wallarr)
        x xi = sx-xi
        y yi = sy-yi
        visit xi yi = (x xi, y yi)
        hwall xi yi = (x xi, y yi*2)
        vwall xi yi = (x xi, y yi*2+1)

        mapline xi yi =
           tile ++ wall ++ next xi
            where
                  tile = show (visits!visit xi yi)
                  wall = show (walls!hwall xi yi)
                  next 0 = "\n"++wallline sx yi
                  next nxi = mapline (nxi-1) yi

        wallline _ 0 =
          "\n"
        wallline xi yi =
          wall ++ show Wall ++ next xi
            where
                  wall = show (walls!vwall xi yi)
                  next 0 = "\n"++mapline sx (yi-1)
                  next nxi = wallline (nxi-1) yi


makeMap :: IntVec2 -> Map
makeMap (sx, sy) =
  Map (makeArray2D (sx, sy*2) Wall) (makeArray2D (sx, sy) Unvisited)

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
moveHead maze pos dst =
  Map  (breakWall (wallarr maze) (wallx, wally))
        (visit (visitarr maze) dst)
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

