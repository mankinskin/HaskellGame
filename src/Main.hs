module Main where
import Input
import Game
import System.IO
import Map

step :: Game -> IO ()
step (Game Quitting _ _) = do return () -- leave gameloop
step game = do
                draw game
                input <- getInput
                step (processInput game input)

main :: IO ()
main = do
        setEcho False
        step initGame
        setEcho True
