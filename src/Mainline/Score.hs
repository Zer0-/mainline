module Mainline.Score
    (calculateScore) where

import Data.Time.Clock.POSIX (POSIXTime)

calculateScore :: Int -> POSIXTime -> Double
calculateScore n t = (e ** ((log maxDbl / endt) * (x - t0)) - 1) * (fromIntegral n)
    where
        maxDbl = 10 ** 300
        t0 = 1572391617
        endt = t0 + (1.577 * 10**10)
        x = realToFrac t
        e = exp 1
