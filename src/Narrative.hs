module Narrative
where

import Game
import Array2D
import Input

import Text.Printf
import Text.Read

import Data.List -- for transpose

printIntro :: IO()
printIntro = do
                putStrLn "###### Rock Collector ######"
                putStrLn "Pickup all the rocks!"
                printHelp
                putStrLn ""


defaultWidth = 25 :: Int
defaultHeight = 10 :: Int

askSize :: IO (IntVec2)
askSize = do
            putStrLn "How big should the world be?"
            printf "Width (default %d): \n" defaultWidth
            sx <- (readOrDefault defaultWidth)
            printf "Height (default %d): \n" defaultHeight
            sy <- (readOrDefault defaultHeight)
            return (sx,sy)


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

