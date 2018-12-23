module Input
where

import System.IO
import Data.Char

ifReadyDo :: IO a -> IO (Maybe a)
ifReadyDo func = hReady stdin >>= f
  where f True = func >>= return . Just
        f _    = return Nothing

type Input = Char

initInput :: IO ()
initInput = setEcho False >> setBuffering NoBuffering

getInput :: IO (Maybe Input)
getInput = getChar >>= return . Just --(ifReadyDo (getChar)) --

setEcho :: Bool -> IO()
setEcho = hSetEcho stdin

setBuffering :: BufferMode -> IO()
setBuffering = hSetBuffering stdin

flush :: IO ()
flush = hFlush stdout
