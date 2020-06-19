{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.TVEx where
import Syntax.Booleans
import Syntax.Num()
import Syntax.Ord
import Data.Maybe
import HStriver
import Lib.Utils
import Theories.Time
import Data.Time
import Data.Aeson
import GHC.Generics
import qualified Prelude as P
import qualified Examples.Cost as Cost(spec,CostTable)

-- // Example TV

data TVStatus = OFF | ON deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)

-- Specs of both examples
spec_no_delays :: Specification
spec_no_delays = [out any_tv_on]

spec_with_delays :: Specification
spec_with_delays = [out exceeded3mPerDay, out exceededAvg3PerDay]

-- Costs of each spec
cost_no_delays :: Cost.CostTable -> Specification
cost_no_delays ct = out (changePointsOf any_tv_on):Cost.spec ct (changePointsOf any_tv_on) -- any_tv_on

-- We could define a minT instead
start_shot :: Stream ()
start_shot = Input "start_shot"

midnight_clock :: Stream TimeTDiff
midnight_clock = let
  ticks = ticksTE start_shot :+ DelayTE Positive midnight_clock
  vals = timeToNextHM (HM 0 0) <$> now
  in "midnight_clock" =: (ticks, vals)

livingroom_tv_status :: Stream TVStatus
livingroom_tv_status = Input "livingroom_tv_status"

office_tv_status :: Stream TVStatus
office_tv_status = Input "office_tv_status"

livingroom_tv_on :: Stream Bool
livingroom_tv_on = let
  ticks = ticksTE livingroom_tv_status
  vals = CV === Leaf ON
  in "livingroom_tv_on" =: (ticks,vals)

office_tv_on :: Stream Bool
office_tv_on = let
  ticks = ticksTE office_tv_status
  vals = CV === Leaf ON
  in "office_tv_on" =: (ticks,vals)

any_tv_on :: Stream Bool
any_tv_on = let
  ticks = ticksTE office_tv_on :+ ticksTE livingroom_tv_on
  vals = office_tv_on @<~ (t?|False) || livingroom_tv_on @<~ (t?|False)
  in "any_tv_on" =: (ticks,vals)

tv_on_in_time :: Stream Bool
tv_on_in_time = let
  timeWithin923 = timeWithin (HM 9 0, HM 23 0)
  ticks = ticksTE any_tv_on :+ ticksTE timeWithin923
  vals = any_tv_on @<~ (t?|False) && timeWithin923 @<~ (t?|False)
  in "tv_on_in_time" =: (ticks,vals)

-- Should be 3h and 3600 instead of 3m 60
exceeded3mPerDay :: Stream ()
exceeded3mPerDay = anticipativeElapse (3*60) (3*60) midnight_clock tv_on_in_time

dayAverage :: Stream TimeTDiff
dayAverage = let
  ticks = ticksTE midnight_clock
  vals = resettableGuardAccum midnight_clock tv_on_in_time @< (t?|0)
  in "dayAverage" =: (ticks,vals)

-- Should be 3h and 3600 instead of 3m 60
exceededAvg3PerDay :: Stream ()
exceededAvg3PerDay = anticipativeElapse (avgNDays 3 @<~ (t?|0)) (avgNDays 3) midnight_clock tv_on_in_time

-- Utils

avgNDays :: Int -> Stream TimeTDiff
avgNDays n = let
  ticks = ticksTE midnight_clock
  last3 = retrieveLast CV n dayAverage
  avg [] = 0
  avg ls = sum ls P./ fromIntegral (length ls)
  vals = avg <$> last3
  in "avgNDays" <: n =: (ticks, vals)

retrieveLast :: (Streamable a, Streamable cv) => ValExpr cv cv -> Int -> Stream a -> ValExpr cv [a]
retrieveLast cv x y = reverse <$> fst (retrieveLast' cv x y)

retrieveLast' :: (Streamable a, Streamable cv) => ValExpr cv cv -> Int -> Stream a -> (ValExpr cv [a], TauExpr a)
retrieveLast' _ 0 str = (Leaf [], t)
retrieveLast' x n str = let
  (vals, e) = retrieveLast' x (n-1) str
  myval = Proj (str:<~e)
  maybeCons (Ev x) l = x:l
  maybeCons _ l = l
  in (maybeCons <$> myval <*> vals, str:<<e)

timeWithin :: TimeInterval -> Stream Bool
timeWithin interval@(start,end) = let
  delayer = "tw_delayer" <: interval =: (ticksTE start_shot :+ DelayTE Positive delayer,delayerval)
  delayerval = min <$> (timeToNextHM start <$> now) <*> (timeToNextHM end <$> now)
  ticks = ticksTE delayer
  vals = (timeToNextHM start <$> now) > (timeToNextHM end <$> now)
  in
  "timeWithin" <: interval =: (ticks,vals)

anticipativeElapse :: (Streamable a, Show b) => ValExpr TimeTDiff TimeTDiff -> b -> Stream a -> Stream Bool -> Stream ()
anticipativeElapse threshold thrstr resetter guard = let
  alarmer = "anticipative_elapse" <: thrstr <: resetter <: guard =: (DelayTE Positive delayer, Leaf ())
  accumulator = resettableGuardAccum resetter guard
  delayer = "ae_delayer" <: thrstr <: resetter <: guard =: (ticksTE accumulator,delayerval)
  getDelayerVal _ False _ = 0 -- Not running
  getDelayerVal accum True thr = thr - accum
  delayerval = getDelayerVal <$> CV <*> guard @<~ (t?|False) <*> threshold
  in alarmer

resettableGuardAccum :: Streamable b => Stream b -> Stream Bool -> Stream TimeTDiff
resettableGuardAccum reset guard = let
  _this = resettableGuardAccum reset guard
  ticks = ticksTE reset :+ ticksTE guard
  (resetting,_) = splitCV
  getAccumVal (Just _) _ _ _ _ _ = Just 0 -- Reset accumulator
  getAccumVal _ oldaccumval (T oldaccumT) False True nowt = -- Was running, now is not
    Just $ oldaccumval + (nowt `diffUTCTime` oldaccumT)
  getAccumVal _ oldaccumval _ True False _ = -- Was not running, now it is
    Just oldaccumval
  getAccumVal _ _ _ _ _ _ = Nothing
  maccum = getAccumVal <$> resetting <*> _this @< (t?|0) <*> Tau (_this :<< t) <*> guard @<~ (t?|False) <*> guard @< (t?|False) <*> now
  accumval = maybenotick maccum
  in
  "RGA" <: reset <: guard =: (ticks, accumval)

-- -=-=-=-=-=-=-=-=-=-=-= OLD Cooking

-- /*item276*/
-- kitchen_cooktop_power :: Stream Integer
-- kitchen_cooktop_power = Input "kitchen_cooktop_power"

-- kitchen_oven_power :: Stream Integer
-- kitchen_oven_power = Input "kitchen_oven_power"

-- kitchen_presence :: Stream Bool
-- kitchen_presence = Input "kitchen_presence"

-- kitchen_cooktop :: Stream Bool
-- kitchen_cooktop = strMap "positive" (P.>0) kitchen_cooktop_power

-- kitchen_oven :: Stream Bool
-- kitchen_oven = strMap "positive" (P.>0) kitchen_oven_power

-- -- TBD. Discuss on the meaning
-- -- define bool ev_kitchen_cooktop_or_kitchen_oven = kitchen_cooktop or kitchen_oven or kitchen_cooktop[1|false] or kitchen_oven[1|false] or kitchen_cooktop[2|false] or kitchen_oven[2|false] or kitchen_cooktop[3|false] or kitchen_oven[3|false] or kitchen_cooktop[4|false] or kitchen_oven[4|false] or kitchen_cooktop[5|false] or kitchen_oven[5|false]
-- ev_kitchen_cooktop_or_kitchen_oven :: Stream Bool
-- ev_kitchen_cooktop_or_kitchen_oven = undefined

-- -- ev     = F_[0,5] (oven \/ cooktop)
-- -- evpast = O_[-5,0] (oven \/ cooktop) <---

-- cooking :: Stream Bool
-- cooking = let
--   ticks = ticksTE kitchen_presence :+ ticksTE ev_kitchen_cooktop_or_kitchen_oven
--   vals = kitchen_presence @<~ (t ?| False) && ev_kitchen_cooktop_or_kitchen_oven @<~ (t ?| False)
--   in "cooking" =: (ticks,vals)

-- -- TBD
-- -- define num sumtimeCooking = if cooking then 1 + sumtimeCooking[-1| 0] else sumtimeCooking[-1| 0]
-- -- define num detectCookingActivity = cooking[-1| false] and not cooking
-- -- define num countCookingActivities = if detectCookingActivity then 1 + countCookingActivities[-1|0] else countCookingActivities[-1|0]
-- -- define num avgAllTimeCooking = sumtimeCooking / countCookingActivities

-- hasStartedCooking :: Stream Bool
-- hasStartedCooking = undefined -- rampup cooking What's rampup?

-- hasStoppedCooking :: Stream Bool
-- hasStoppedCooking = undefined -- rampup cooking What's rampup?

-- sumtimeCooking :: (Streamable a) => TickExpr a -> Stream Duration
-- sumtimeCooking ticks = let
--   vals = diffUTCTime <$> (toUTCTime <$> Tau t) <*> (toUTCTime <$> Tau (hasStartedCooking  :<< t))
--   in "sumtimecook" =: (ticks,vals)

-- --- Now sumtimeCooking can be instantiated with hasStoppedCooking ticks
-- --- to obtain a stream cookingDuration
-- cookingDuration :: Stream Duration
-- cookingDuration = undefined --  sumtimeCooking hasStoppedCooking.ticks I didn't --  get it

-- lunchhours :: TimeInterval
-- lunchhours = (read "11:00", read "16:00")
-- dinnerhours :: TimeInterval
-- dinnerhours = (read "19:00", read "23:00")

-- tickerinput :: Stream ()
-- tickerinput = undefined

-- dinnerperiod :: Stream Bool
-- dinnerperiod = let
--   ticks = ticksTE tickerinput
--   vals = inInterval <$> (unT <$> Tau t) <*> Leaf dinnerhours
--   in "dinnerperiod" =: (ticks,vals)

-- Revisit these definitions
-- define num cookingtoday = if day(now) == day(daystart) then sumtimeCooking else cookingtoday[0|0]
-- define num yesterday = prev(day(now))
-- define num cookingyesterday = sumtimeCooking[yesterday] //idea

-- define num weekdaysumcooking = if weekday(day(now)) then weekdaysumcooking + sumcooking[today] else weekdaysumcooking
