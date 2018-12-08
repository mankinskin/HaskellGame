module Input
where

import System.IO
import Game


ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo handle func = hReady handle >>= f
  where f True = func >>= return . Just
        f _    = return Nothing

type Input = String

quitCharacters :: [String]
quitCharacters = ["q", ":Q", ":q","quit","exit"]

getInput :: IO (Maybe Input)
getInput = getLine >>= return . Just --(stdin `ifReadyDo` getLine)

processInput :: Game -> Maybe Input -> Game
processInput game Nothing = game
processInput game (Just input)
                      | input `elem` quitCharacters = Game Quitting (screen game) (time game)
                      | otherwise = game

setEcho :: Bool -> IO()
setEcho b = hSetEcho stdin b
