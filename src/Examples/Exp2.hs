{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.Exp2 where
import Syntax.Num((/))
import HStriver
import Examples.TVEx

spec :: Specification
spec = [out exceeded3hPerDay, out exceededAvgPlus30m]

exceeded3hPerDay :: Stream ()
exceeded3hPerDay = anticipativeElapse (3*3600) (3*3600) midnight_clock tv_on_in_time

exceededAvgPlus30m :: Stream ()
exceededAvgPlus30m = anticipativeElapse (avgHist @<~ (t?|0)+30*60) avgHist midnight_clock tv_on_in_time

-- Utils
daysCounter :: Stream Int
daysCounter = "daysCounter" =: let
  ticks = ticksTE midnight_clock
  vals = daysCounter @< (t?|0) + 1
  in (ticks,vals)

avgHist :: Stream TimeTDiff
avgHist = "avgHist" =: let
  ticks = ticksTE dayAverage
  prevAvg = avgHist @< (t?|0)
  days = fromIntegral <$> daysCounter @<~ (t?|undefined)
  vals = (prevAvg * (days-1) + CV) / days
  in (ticks,vals)
