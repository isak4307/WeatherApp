{-# LANGUAGE ScopedTypeVariables #-}

module TUtils where

import Data.Time
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Utils

testUtils :: IO ()
testUtils = do
  -- Test that a UTCTime will be rounded to the nearest quarter which is in the list of quarterTimes
  quickCheck test_roundToNearestQuarter
  -- Test that a UTCTime rounded to an hour will always have 0 minutes.
  quickCheck test_roundToExactlyHour

-- | Check that the time will always be one of the quarters in 24 hours
test_roundToNearestQuarter :: UTCTime -> Bool
test_roundToNearestQuarter t =
  let roundedT = roundToNearestQuarter t
      roundedSeconds = round (utctDayTime roundedT) `mod` (24 * 3600)
   in roundedSeconds `elem` quarterTimes

-- | Check that the time that is converted is on the hour marker aka 0 minutes
test_roundToExactlyHour :: UTCTime -> Bool
test_roundToExactlyHour t =
  let roundedT = roundToNearestHour t
      secondsPerHour = 3600 :: Int
      seconds = round (utctDayTime roundedT) :: Int
   in -- It should give 0 since the seconds are exactly 0 minutes when it is a whole hour
      seconds `mod` secondsPerHour == 0