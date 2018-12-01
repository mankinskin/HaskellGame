import Input
import Game

draw :: Game -> IO ()
draw g = do
          putStrLn (snd g)
          return ()


gameloop :: Game -> IO ()
gameloop (False, _) = do return () -- leave gameloop
gameloop game = do
                draw game
                input <- getInput
                gameloop (processInput game input)

main :: IO ()
main = gameloop (True, "Beginning")
