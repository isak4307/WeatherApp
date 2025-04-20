{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TUtils where

import qualified Data.Text as T
import Data.Time
import GenUtils
import Test.QuickCheck
import Utils
-- TODO have to test more functions, don't know what functions to test that makes sense/ isn't too complicated
testUtils :: IO ()
testUtils = quickCheck $ forAll genUTCTime $ \t -> test_roundToNearestQuarter t

test_roundToNearestQuarter :: UTCTime -> Bool
test_roundToNearestQuarter t =
  let roundedT = roundToNearestQuarter t
      roundedSeconds = round (utctDayTime roundedT) `mod` (24 * 3600)
   in roundedSeconds `elem` targetTimes

genUTCTime :: Gen UTCTime
genUTCTime = do
  date <- dateGen
  let parsedTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack date) :: Maybe UTCTime
  case parsedTime of
    Just utcTime -> return utcTime
    Nothing -> error "Generated text could not be parsed into UTCTime"
