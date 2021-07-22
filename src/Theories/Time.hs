module Theories.Time where

import Data.Time

data HoursMinutes = HM {hours:: Int, minutes :: Int} deriving Show
type TimeInterval = (HoursMinutes,HoursMinutes)

timeToNextHM :: HoursMinutes -> UTCTime -> NominalDiffTime
timeToNextHM (HM h m) now  = let
  seconds = fromRational.toRational $ utctDayTime now
  targetSeconds = fromIntegral $ h*3600 + m*60
  diff = targetSeconds - seconds
  in diff + if diff>0 then 0 else 24*3600

-- From SO
-- import Data.Char
-- instance Read HoursMinutes where
--   readsPrec _ input =
--     let (hours,rest1) = span isDigit input
--         hour = read hours :: Int
--         (c:rest2) = rest1
--         (mins,rest3) = splitAt 2 rest2
--         minute = read mins :: Int
--         in
--       if c==':' && all isDigit mins && length mins == 2 then -- it looks valid
--          [(HM hour minute,rest3)]
--        else []                      -- don't give any parse if it was invalid
