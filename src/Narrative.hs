module Narrative
where

import Game
import Array2D
import Input

import Text.Printf

import Data.List -- for transpose

printIntro :: IO()
printIntro = do
                putStrLn "###### Maze Game ######"

askSize :: IO (IntVec2)
askSize = do
            putStrLn "How big should the world be?"
            printf "Width (default %d): \n" defx
            sx <- (readOrDefault defx)
            printf "Height (default %d): \n" defy
            sy <- (readOrDefault defy)
            return (sx,sy)
          where
            defx = 15 :: Int
            defy = 5 :: Int

printHelp :: IO()
printHelp = putStrLn .foldr (++) " to quit.\n" $("Type ":list)
  where
    list = foldr (:) ("or ":[last quitCmds]) $merged
    merged = concat . transpose $[take tosep quitCmds, seperators]
    tosep = length quitCmds-2
    seperators = take tosep.cycle $[", "]

