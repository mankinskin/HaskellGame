module Main where

import Time
import Input
import Game

import System.IO

frameloop :: Game -> IO ()
frameloop (Game Quitting _ _) = do return () -- leave
frameloop game = do   -- keep running
                    update game
                    (getInput >>= (processInput . return $game)) >>= frameloop

main :: IO ()
main = do
          starttime <- timeSinceEpoch
          frameloop (initGame (30, 10) starttime)
