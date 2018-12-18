module Game
where

import Time
import Map
import MazeGen
import Array2D
import World
import Player

data GameState = Quitting | Running deriving (Show)

data Game = Game { state::GameState, world::World, time::GameTime }

data GameTime = GameTime { start::Milliseconds, duration::Milliseconds }

updateTime :: Game -> IO Game
updateTime game =
  do
    currentTime <- timeSinceEpoch
    return (Game (state game) (world game) (GameTime startMS (currentTime - startMS)))
      where startMS = start.time $game

initGame :: IntVec2 -> Integer -> Game
initGame size seed =
    Game Running (World (genMaze size seed) (Player (0, 0))) (GameTime 0 0)

update :: Game -> IO()
update g = updateTime g >>= draw

draw :: Game -> IO()
draw g = do
            putStr.show$(world g)
            return ()

quitGame :: Game -> Game
quitGame (Game _ s t) = Game Quitting s t

