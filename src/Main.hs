module Main where
import Input
import Game
import System.IO
import Map

frameloop :: Game -> IO ()
frameloop (Game Quitting _ _) = do return () -- leave
frameloop game = do   -- keep running
                    draw game
                    (getInput >>= (processInput . return $game)) >>= frameloop

main :: IO ()
main = frameloop initGame
