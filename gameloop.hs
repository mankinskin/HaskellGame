import System.IO


ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo handle func = hReady handle >>= f
  where f True = func >>= return . Just
        f _    = return Nothing

type Input = String

quitCharacters :: [String]
quitCharacters = ["q", ":Q", ":q","quit","exit"]

getInput :: IO (Maybe Input)
getInput = (stdin `ifReadyDo` getLine)

type Game = (Bool, String)

draw :: Game -> IO ()
draw g = do
          putStrLn (snd g)
          return ()

quit :: Game -> Game
quit g = g

processInput :: Game -> Maybe Input -> Game
processInput game Nothing = game
processInput game (Just input)
                      | input `elem` quitCharacters = let (_, str) = game in (False, str)
                      | otherwise = game

step :: Game -> IO ()
step (False, _) = return ()
step game = do
                putStrLn "step!"
                --draw game
                input <- getInput
                step (processInput game input)


game :: IO ()
game = step (True, "Beginning")

main = game
