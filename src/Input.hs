module Input
where

import Game
import World
import Direction

import System.IO
import Text.Read
import Data.Char

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo handle func = hReady handle >>= f
  where f True = func >>= return . Just
        f _    = return Nothing

type Input = String

lowerString :: String -> String
lowerString str = [toLower c | c <- str]

getInput :: IO (Maybe Input)
getInput = getLine >>= return . Just . lowerString --(stdin `ifReadyDo` getLine)--

setEcho :: Bool -> IO()
setEcho b = hSetEcho stdin b

readOrDefault :: Int -> IO (Int)
readOrDefault def =
            do
              str <- getLine
              case str of
                "" -> return def
                _ -> do
                       tryval <- tryReadInt str
                       case tryval of
                         Nothing -> readOrDefault def
                         (Just a) -> return a

tryReadInt :: String -> IO (Maybe Int)
tryReadInt str = case (readEither str :: (Either String Int)) of
                  Left e -> do
                              putStrLn.show$e
                              return Nothing
                  Right a -> return (Just a)

