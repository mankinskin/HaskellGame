module Game
where

import Data.Word
import Data.Time.Clock.POSIX

data GameState = Quitting | Running deriving (Show)

data GameTime = GameTime { start::Word64, duration::Word64 }
data Game = Game { state::GameState, screen::String, time::GameTime }

timeSinceEpoch :: IO (Word64)
timeSinceEpoch = round `fmap` getPOSIXTime

updateMS :: Game -> IO Game
updateMS game =
  do
    currentTime <- timeSinceEpoch
    return (Game (state game) (screen game) (GameTime startMS (currentTime - startMS)))
      where startMS = start . time $ game

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
    --currentTime <- timeSinceEpoch
    Game Running initScreen (GameTime 0 0)

draw :: Game -> IO()
draw g = do
            putStr (screen g)
            putStrLn (take screenWidth $ cycle "-")
            return ()

