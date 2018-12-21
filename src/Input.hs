module Input
where

import Game
import World

import System.IO
import Data.List
import Direction


ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo handle func = hReady handle >>= f
  where f True = func >>= return . Just
        f _    = return Nothing

type Input = String

quitCmds :: [String]
quitCmds = ["q", ":Q", ":q","quit","exit"]

getInput :: IO (Maybe Input)
getInput = getLine >>= return . Just --(stdin `ifReadyDo` getLine)--

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

setEcho :: Bool -> IO()
setEcho b = hSetEcho stdin b

printHelp :: IO()
printHelp = putStrLn .foldr (++) " to quit.\n" $("Type ":list)
  where
    list = foldr (:) ("or ":[last quitCmds]) $merged
    merged = concat . transpose $[take tosep quitCmds, seperators]
    tosep = length quitCmds-2
    seperators = take tosep.cycle $[", "]


