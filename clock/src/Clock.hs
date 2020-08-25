module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int -- minutes
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour * 60  + min) `mod` (24 * 60))

toString :: Clock -> String
toString (Clock c) = let (h,m) = c `quotRem` 60
                     in printf "%02d:%02d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock c) = fromHourMin hour (min + c)
