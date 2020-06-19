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

-- TODO make triple-equalizable class
isticking :: (Streamable b) => Stream a -> ValExpr b Bool
isticking x = Tau (x:<~t) === Tau t

-- firstspec
{-
input int in1, in2
output int x, r, s

r.ticks = in2.ticks
r.val t = r(<t|0) + cv

x.ticks = in1.ticks
x.val t = r(~t)

s.ticks = r.ticks
s.val t = cv + x(t~)

in2 = [(0,0), (2,2), (4,4), (6,6), (8,8),...]
in1 = [(10, 10)]
r = [(0, 0), (2,2), (4, 6), (6, 12), (8, 20), (10, 30),...]
x = [(10, 30)]
s = [(0, 30), (2,32), (4, 36), (6, 42), (8, 50), (10, 60),...]
-}

-- It breaks because x>>t is posOutside at a certain point

firstspec = [out in1, out in2, out r, out x, out s]

in1 :: Stream Int
in1 = Input "in1"
in2 :: Stream Int
in2 = Input "in2"

r :: Stream Int
r =
  let
    ticks = ticksTE in2
    vals = (+) <$> r @< (t?|0) <*> CV
  in "r" =: (ticks, vals)

x :: Stream Int
x =
  let
    ticks = ticksTE in1
    vals = r @<~ t
  in "x" =: (ticks, vals)

s :: Stream Int
s =
  let
    ticks = ticksTE r
    vals = (+) <$> x @>(t?|0) <*> CV
  in "s" =: (ticks, vals)

-- daspec

-- daspec :: Specification
-- daspec = [out i0, out o0, out ointernal, out shifti0]

-- i0 :: Stream Int
-- i0 = Input "i0"

-- o0 :: Stream Int
-- o0 = let
--     ticks = ticksTE i0 :+ ticksTE ointernal
--     vals = mysum <$> CV
--   in "o0" =: (ticks, vals)

-- ointernal :: Stream Int
-- ointernal = let
--     ticks = ConstTE 40
--     vals = timeToInt <$> Tau (i0 :>> t)
--    in "ointernal" =: (ticks, vals)

-- shifti0 :: Stream Int
-- shifti0 = let
--     ticks = ShiftTE 5 i0
--     vals = CV
--   in "shifti0" =: (ticks, vals)


-- mysum :: (Maybe Int, Maybe Int) -> Int
-- mysum (ma, mb) = let
--   a = fromMaybe 0 ma
--   b = fromMaybe 0 mb
--   in a+b

-- timeToInt :: Time -> Int
-- timeToInt (T x) = round x + 4000
-- timeToInt PosInfty = 9999999

-- maybeIntToInt :: MaybeOutside Int -> Int
-- maybeIntToInt NegOutside = -1
-- maybeIntToInt PosOutside = 9999999
-- maybeIntToInt (Ev x) = x
