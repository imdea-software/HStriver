{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.Cost where
import Syntax.Booleans
import Syntax.Num()
import Data.Maybe
import HStriver
import Data.Time
import Data.Aeson
import GHC.Generics

data RunMode = Alert | Sleeping  deriving (Show,Generic,ToJSON,Eq)

type Cost = Int

data CostTable = CT {
  alertPatience :: TimeDiff,
  alertPerSecond :: Cost,
  sleepingPerSecond :: Cost,
  processEvent :: Cost,
  wakeUp :: Cost,
  gotoSleep :: Cost}

runCostPerSecond :: RunMode -> CostTable -> Cost
runCostPerSecond Alert = alertPerSecond
runCostPerSecond Sleeping = sleepingPerSecond

transitionCost :: CostTable -> RunMode -> RunMode -> Cost
transitionCost ct Alert Alert = processEvent ct -- Cost of processing an event
transitionCost ct Sleeping Alert = wakeUp ct + processEvent ct -- Wakeup + processing
transitionCost ct Alert Sleeping = gotoSleep ct -- Cost of going to sleep
transitionCost _ Sleeping Sleeping = error "going to sleep while sleeping?" -- This should never happen

spec :: Typeable a => CostTable -> Stream a -> Specification
spec ct wakeup = [out$costJumps ct wakeup, out$runMode (alertPatience ct) wakeup]

sleep :: Typeable a => TimeDiff -> Stream a -> Stream ()
sleep patience wakeup = let
  ticks = DelayTE Positive sleep_delayer
  vals = Leaf ()
  sleep_delayer = "sleep_delayer" =: (ticksTE wakeup, Leaf patience)
  in "sleep" =: (ticks,vals)

runMode :: Typeable a => TimeDiff -> Stream a -> Stream RunMode
runMode patience wakeup = let
  ticks = ticksTE wakeup :+ ticksTE (sleep patience wakeup)
  wakingup = isJust.fst <$> CV
  vals = if wakingup then Leaf Alert else Leaf Sleeping
  in "runMode" =: (ticks,vals)

costJumps :: Typeable a => CostTable -> Stream a -> Stream (Cost,Cost)
costJumps ct wakeup = let
  _this = costJumps ct wakeup
  rmStream = runMode (alertPatience ct) wakeup
  previousRunMode = rmStream @< (t?|Alert) -- To only count the cost of processing the first event
  currentRunMode = CV
  costOfTransitioning = transitionCost ct <$> previousRunMode <*> currentRunMode
  prevt = getTimeT <$> Tau (rmStream :<< t) <*> now
  getTimeT (T x) _ = x
  getTimeT _ y = y
  timediff = round.realToFrac <$> (tDiff <$> now <*> prevt)
  ---
  ticks = ticksTE rmStream
  accum = (snd<$>_this @< (t?|(0,0))) + timediff * (flip runCostPerSecond ct <$> previousRunMode)
  vals = (,) <$> accum <*> accum + costOfTransitioning
  in "cost" =: (ticks,vals)
