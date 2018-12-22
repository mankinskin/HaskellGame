module Main
where

import Time
import Input
import Game
import World
import Narrative
import Direction

import System.IO

main :: IO ()
main = do
          printIntro
          starttime <- timeSinceEpoch
          (askSize >>= return . ((flip initGame) starttime)) >>= frameloop

frameloop :: Game -> IO ()
frameloop (Game Quitting _ _) = do return () -- leave
frameloop game = do   -- keep running
                    update game
                    (getInput >>= (processInput . return $game)) >>= frameloop

processInput :: IO Game -> Maybe Input -> IO Game
processInput game Nothing = game
processInput game (Just input)
                      | input `elem` quitCmds = game >>= return . quitGame
                      | input `elem` moveCmds = game >>= return . (processMove input)
                      | otherwise = printHelp >> game

processWorld :: Input -> World -> World
processWorld input w
  | input == moveCmds!!0 = move w North
  | input == moveCmds!!1 = move w East
  | input == moveCmds!!2 = move w South
  | input == moveCmds!!3 = move w West
  | otherwise = w

processMove :: Input -> Game -> Game
processMove input game =
  Game (state game) (processWorld input (world game)) (time game)
