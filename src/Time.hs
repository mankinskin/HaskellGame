module Time where


import Data.Time.Clock.POSIX

type Milliseconds = Integer

timeSinceEpoch :: IO (Milliseconds)
timeSinceEpoch = round `fmap` getPOSIXTime

