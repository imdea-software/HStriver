{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.Exp1 where
import Syntax.Booleans
import Syntax.Num()
import Syntax.Ord
import HStriver
import Data.Time
import Examples.TVEx

spec :: Specification
spec = [out exceeded3hPerDay, out exceededAvgPlus30m]

ticks :: TickExpr Bool
ticks = ticksTE any_tv_on

instantN :: Stream Int
instantN = "instantN" =: let
  vals = instantN @< (t?|0) + 1
  in (ticks,vals)

isNewDay :: Stream Bool
isNewDay = "isNewDay" =: let
  today = utctDay <$> now
  prev = utctDay.unT <$> Tau (any_tv_on :<< t)
  vals = if nowVal instantN === 1 then Leaf True else (/=) <$> today <*> prev
  in (ticks,vals)

instantsPerMinute :: Streamable a => ValExpr a Int
instantsPerMinute = 1

nowVal :: (Streamable cv,Streamable a) => Declaration a -> ValExpr cv a
nowVal x = x @<~ (t?|undefined)

-- This counts instants
howMuchTvToday :: Stream Int
howMuchTvToday = "howMuchTvToday" =: let
  prevVal = if nowVal isNewDay then 0 else howMuchTvToday @< (t?|0)
  sumVal = if nowVal any_tv_on then 1 else 0
  vals = prevVal + sumVal
  in (ticks,vals)

exceeded3hPerDay :: Stream Bool
exceeded3hPerDay =
  "exceeded3hPerDay" =: let
  vals = nowVal howMuchTvToday > 3*60*instantsPerMinute
  in (ticks,vals)

countDays :: Stream Int
countDays = "countDays" =: let
  vals = countDays @< (t?|0) + if nowVal isNewDay then 1 else 0
  in (ticks,vals)

-- This counts instants
totalTVTime :: Stream Int
totalTVTime = "totalTVTime" =: let
  vals = totalTVTime @<(t?|0) + if nowVal any_tv_on then 1 else 0
  in (ticks,vals)

-- This counts instants
avgTvPast :: Stream Int
avgTvPast =
  "avgTVPast" =: let
  vals = if nowVal isNewDay then div <$> nowVal totalTVTime <*> nowVal countDays else avgTvPast @< (t?|0)
  in (ticks,vals)

exceededAvgPlus30m :: Stream Bool
exceededAvgPlus30m =
  "exceededAvgPlus30m" =: let
  vals = nowVal howMuchTvToday > nowVal avgTvPast + 30 * instantsPerMinute
  in (ticks,vals)
