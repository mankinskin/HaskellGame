module Game
where

import Time

data GameState = Quitting | Running deriving (Show)

data GameTime = GameTime { start::Milliseconds, duration::Milliseconds }
data Game = Game { state::GameState, screen::String, time::GameTime }

updateMS :: Game -> IO Game
updateMS game =
  do
    currentTime <- timeSinceEpoch
    return (Game (state game) (screen game) (GameTime startMS (currentTime - startMS)))
      where startMS = start.time $game

screenWidth :: Int
screenWidth = 100

screenHeight :: Int
screenHeight = 40

initScreen :: String
initScreen = take (screenHeight*(screenWidth+1)) (cycle line)
  where
    line :: String
    line = 'O':(take (screenWidth-1) $ cycle [' ']) ++ "\n"

initGame :: Game
initGame =
    Game Running initScreen (GameTime 0 0)

draw :: Game -> IO()
draw g = do
            putStr (screen g)
            putStrLn (take screenWidth $ cycle "-")
            return ()

quitGame :: Game -> Game
quitGame (Game _ s t) = Game Quitting s t

