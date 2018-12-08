module Time where


import Data.Word
import Data.Time.Clock.POSIX

type Milliseconds = Word64

timeSinceEpoch :: IO (Milliseconds)
timeSinceEpoch = round `fmap` getPOSIXTime

