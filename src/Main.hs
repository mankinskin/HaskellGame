module Main
where

import Time
import Input
import Game
import World
import Narrative

import System.IO

main :: IO ()
main = do
          printIntro
          size <- askSize
          starttime <- timeSinceEpoch
          initInput
          draw (initGame size starttime) >>= frameloop

frameloop :: Game -> IO ()
frameloop (Game Quitting _ _) = do return () -- leave
frameloop game = do   -- keep running
                  flush >> (getInput >>= (update . return $game)) >>= frameloop

