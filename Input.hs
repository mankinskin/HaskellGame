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
getInput = (stdin `ifReadyDo` getLine)

processInput :: Game -> Maybe Input -> Game
processInput game Nothing = game
processInput game (Just input)
                      | input `elem` quitCharacters = let (_, str) = game in (False, str)
                      | otherwise = game

