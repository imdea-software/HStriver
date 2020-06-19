{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.Exp3and4 where
import Syntax.Booleans
import Syntax.Num()
import HStriver
import Data.Time
import Examples.TVEx
import Theories.Ambient

-- For experiment 3:

spec3 :: Specification
spec3 = [out any_tv_on, out totalTVTime]

totalTVTime :: Stream NominalDiffTime
totalTVTime = "totalTVTime" =: let
  ticks = ticksTE any_tv_on
  prev_any_tv = unT <$> Tau (any_tv_on :<< t)
  vals = totalTVTime @< (t?|0) + if any_tv_on @< (t?|False) then diffUTCTime <$> now <*> prev_any_tv else 0
  in (ticks,vals)

-- anytvon remains the same

-- For experiment 4:

spec4 :: Specification
spec4 = [out totalReporter]

firstHour :: TimeT
firstHour = sysTimeToTimeT False $ SysTime 2017 0 30 7 00 00

roundHourTicker :: Stream TimeTDiff
roundHourTicker = "roundHourTicker" =: let
  ticks = ConstTE firstHour :+ DelayTE Positive roundHourTicker
  vals = 3600
  in (ticks,vals)

totalReporter :: Stream NominalDiffTime
totalReporter = "totalReporter" =: let
  ticks = ticksTE roundHourTicker
  vals = totalTVTime @<~(t?|0)
  in (ticks,vals)
