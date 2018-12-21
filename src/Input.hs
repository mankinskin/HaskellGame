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

quitCommands :: [String]
quitCommands = ["q", ":Q", ":q","quit","exit"]

getInput :: IO (Maybe Input)
getInput = getLine >>= return . Just --(stdin `ifReadyDo` getLine)

processInput :: IO Game -> Maybe Input -> IO Game
processInput game Nothing = game
processInput game (Just input)
                      | input `elem` quitCommands = game >>= return . quitGame
                      | input `elem` dirStrings = game >>= return . (processMove input)
                      | otherwise = printHelp >> game

processMove :: Input -> Game -> Game
processMove input game
  | input == dirStrings!!0 =
    Game (state game) (move (world game) North) (time game)
  | input == dirStrings!!1 =
    Game (state game) (move (world game) East) (time game)
  | input == dirStrings!!2 =
    Game (state game) (move (world game) South) (time game)
  | input == dirStrings!!3 =
    Game (state game) (move (world game) West) (time game)
  | otherwise = game

setEcho :: Bool -> IO()
setEcho b = hSetEcho stdin b

printHelp :: IO()
printHelp = putStrLn .foldr (++) " to quit.\n" $("Type ":list)
  where
    list = foldr (:) ("or ":[last quitCommands]) $merged
    merged = concat . transpose $[take tosep quitCommands, seperators]
    tosep = length quitCommands-2
    seperators = take tosep.cycle $[", "]


