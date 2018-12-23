module Game
where

import Time
import Map
import Array2D
import World
import Player
import System.Random
import Input
import Direction
import Data.List
import Text.Printf
import Item

quitCmd :: Input
quitCmd = 'q'

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
initGame size time =
    Game Running (genWorld size (mkStdGen.fromInteger$time)) (GameTime time 0)

update :: IO Game -> (Maybe Input) -> IO Game
update g Nothing = g >>= updateTime
update g (Just i) = (g >>= updateTime >>= processInput i)

draw :: Game -> IO Game
draw g = do
            putStr.show$(world g)
            return g

quitGame :: Game -> Game
quitGame (Game _ s t) = Game Quitting s t

processInput :: Input -> Game -> IO Game
processInput input game@(Game st w t)
        | input == quitCmd = return . quitGame $game
        | input `elem` moveCmds = draw (Game st (processMove input w) t)
        | input == actionCmd = draw (Game st (World.interact w) t)
        | otherwise = printHelp >> return game

processMove :: Input -> World -> World
processMove input w
  | input == northCmd = move w North
  | input == eastCmd = move w East
  | input == southCmd = move w South
  | input == westCmd = move w West
  | otherwise = w

printHelp :: IO()
printHelp = do
              printf "Usage:\n"
              printf "%c - quit\n" quitCmd
              printf "%c - move up\n" northCmd
              printf "%c - move right\n" eastCmd
              printf "%c - move down\n" southCmd
              printf "%c - move left\n" westCmd
              printf "%c - pick up item\n" actionCmd
              printf "Type anything else to show help\n"
