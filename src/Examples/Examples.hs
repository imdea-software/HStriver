{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.Examples where
import Syntax.Booleans
import Syntax.Num()
import Data.Maybe
import HStriver

-- Example 1 from Striver paper

-- ticks stock := sale.ticks U arrival.ticks
-- define int stock := stock(<t,0) +
-- (if isticking(arrival) then arrival(~t) else 0) - (if isticking(sale ) then sale(~t ) else 0)

paper1 :: Specification
paper1 = [out stock]

sale :: Stream Int
sale = Input "sale"

arrival :: Stream Int
arrival = Input "arrival"

stock :: Stream Int
stock = let
  ticks = ticksTE sale :+ ticksTE arrival
  vals = stock @< (t?|0) +
    (if isticking arrival then arrival @<~ t else 0) -
    (if isticking sale then sale @<~ t else 0)
  in
  "stock" =: (ticks, vals)

isticking :: (Streamable b) => Stream a -> ValExpr b Bool
isticking x = Tau (x:<~t) === Tau t
