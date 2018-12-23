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
                      | input `elem` moveCmds = game >>= return . (processWorld input)
                      | otherwise = printHelp >> game

processMove :: Input -> World -> World
processMove input w
  | input `elem` northCmds = move w North
  | input `elem` eastCmds = move w East
  | input `elem` southCmds = move w South
  | input `elem` westCmds = move w West
  | otherwise = w

processWorld :: Input -> Game -> Game
processWorld input game =
  Game (state game) (processMove input (world game)) (time game)
