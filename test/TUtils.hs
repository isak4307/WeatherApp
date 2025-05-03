{-# LANGUAGE ScopedTypeVariables #-}

module TUtils where

import Data.Time
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Utils

testUtils :: IO ()
testUtils = do
  quickCheck test_roundToNearestQuarter
  quickCheck test_roundToExactlyHour

-- | Check that the time will always be one of the quarters in 24 hours
test_roundToNearestQuarter :: UTCTime -> Bool
test_roundToNearestQuarter t =
  let roundedT = roundToNearestQuarter t
      roundedSeconds = round (utctDayTime roundedT) `mod` (24 * 3600)
   in roundedSeconds `elem` targetTimes

-- | Check that the time that is converted is on the hour marker aka 0 minutes
test_roundToExactlyHour :: UTCTime -> Bool
test_roundToExactlyHour t =
  let roundedT = roundToNearestHour t
      secondsPerHour = 3600 :: Int
      seconds = round (utctDayTime roundedT) :: Int
   in -- It should give 0 since the seconds are exactly 0 when it is a whole hour
      seconds `mod` secondsPerHour == 0