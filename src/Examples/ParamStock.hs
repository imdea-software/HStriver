{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.ParamStock where
import Syntax.Booleans
import Syntax.Num()
import Data.Maybe
import Lib.Utils
import qualified Prelude as P ((<))
import HStriver

-- Example 1 from Striver paper

-- ticks stock := sale.ticks U arrival.ticks
-- define int stock := stock(<t,0) +
-- (if isticking(arrival) then arrival(~t) else 0) - (if isticking(sale ) then sale(~t ) else 0)

data Product = ProductA | ProductB | ProductC deriving Show

paper1 :: Specification
paper1 = [out any_alarm]

sale :: Product -> Stream Int
sale p = Input $ "sale" <: p

arrival :: Product -> Stream Int
arrival p = Input $ "arrival" <: p

stock :: Product -> Stream Int
stock p = let
  ticks = ticksTE (sale p) :+ ticksTE (arrival p)
  vals = stock p @< (t?|0) +
    (if isticking (arrival p) then (arrival p) @<~ t else 0) -
    (if isticking (sale p) then (sale p) @<~ t else 0)
  in
  "stock" <:p =: (ticks, vals)

low_stock :: Product -> Stream Bool
low_stock p = strMap "low" (P.< lowval p) (stock p)
  where 
  lowval ProductA = 20
  lowval ProductB = 100
  lowval ProductC = 200

cp_low_stock :: Product -> Stream Bool
cp_low_stock p = changePointsOf (low_stock p)

alarm_timer :: Product -> Stream TimeDiff
alarm_timer p = let
  ticks = ticksTE $ cp_low_stock p
  val = if CV then 50 else (-1)
  in "alarm_timer" <: p =: (ticks,val)

alarm :: Product -> Stream ()
alarm p = let
  ticks = DelayTE Positive (alarm_timer p)
  val = Leaf ()
  in "alarm" <: p =: (ticks,val)

any_alarm :: Stream ()
any_alarm = let
  ticks = ticksTE (low_stock ProductA) :+ ticksTE (low_stock ProductB) :+ ticksTE (low_stock ProductC)
  val = Leaf ()
  in "any_alarm" =: (ticks,val)

alarm_timer2 :: Product -> Stream TimeDiff
alarm_timer2 p = let
  ticks = ticksTE (cp_low_stock p) :+ ticksTE (arrival p)
  (mls, marr) = splitCV
  ls = fromJust <$> mls
  val = if (isJust <$> marr) || not ls then (-1) else 50
  in "alarm_timer2" <: p =: (ticks,val)

isticking :: (Streamable b) => Stream a -> ValExpr b Bool
isticking x = Tau (x:<~t) === Tau t
