{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Ambient where
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Maybe
import HStriver(TimeT, TSGetter)
import Data.Time

data SysTime = SysTime {year :: Integer, month :: Int, day :: Int, hour :: Int, minute :: Int, second :: Double} deriving (Generic, FromJSON)

sysTimeToTimeT :: Bool -> SysTime -> TimeT
sysTimeToTimeT ignoreSeconds (SysTime y mo d h m s) = let
  today = fromGregorian y (mo+1) d
  now False = UTCTime today (secondsToDiffTime.toInteger $ h*3600 + m*60 + round s)
  now True = UTCTime today (secondsToDiffTime.toInteger $ h*3600 + m*60)
  in now ignoreSeconds

sysTimeGetter :: TSGetter
sysTimeGetter m = (sysTimeToTimeT False .fromJust.parseMaybe parseJSON) (m Map.! "Time")

sysTimeGetterIgnoreSeconds :: TSGetter
sysTimeGetterIgnoreSeconds m = (sysTimeToTimeT True .fromJust.parseMaybe parseJSON) (m Map.! "Time")
