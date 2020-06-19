{-# LANGUAGE RebindableSyntax  #-}
module Examples.RobustSTLSpec where
import HStriver
import Lib.RobustSTL as RobustSTL

spec :: Specification
spec = [out daphis, out phi, out psi]

phi :: Stream StrVal
phi = Input "phi" -- [(fromIntegral x,Just $ toDyn (P.mod y 10)) | (x,y) <- zip [0..50000000] (randoms $ mkStdGen 1 :: [Int])]

psi :: Stream StrVal
psi = Input "psi" -- [(fromIntegral x + 0.5,Just $ toDyn (P.mod y 10)) | (x,y) <- zip [0..50000000] (randoms $ mkStdGen 2 :: [Int])]

daphis :: Declaration StrVal
daphis = until (0,5) phi psi

-- OLD
-- max_speed = 5
-- ok_speed = 4
-- offsets :: Declaration Double
-- offsets = let
--     ticks = ticksTE inps
--     vals = let
--       sgn = if (offsets :<< t ?| 0)<0 then -1 else 1
--       in CV - Leaf 0.5 + Leaf 0.3 * sgn
--   in Out "offsets" ticks vals

-- speed :: Declaration Double
-- speed = let
--     ticks = ticksTE offsets
--     vals = (speed :<< t ?| 0) + CV
--   in Out "speed" ticks vals

-- toofast :: Declaration Bool
-- toofast = let
--     ticks = ticksTE speed
--     vals = CV > max_speed
--   in Out "toofast" ticks vals

-- speedok :: Declaration Bool
-- speedok = let
--     ticks = ticksTE speed
--     vals = CV <= ok_speed
--   in Out "speedok" ticks vals

-- decel :: Declaration Bool
-- decel = let
--     ticks = ticksTE speed
--     vals = CV > projGet(speed :>> t)
--   in Out "decel" ticks vals

-- theuntil = STL.until (0,5) decel speedok

-- slow_down :: Declaration Bool
-- slow_down = let
--   ticks = ticksTE theuntil
--   vals = CV
--   in Out "slow_down" ticks vals

-- ok :: Declaration Bool
-- ok = let
--   ticks = ticksTE toofast :+ ticksTE slow_down
--   vals = (toofast :<~ t ?| False) `implies` (slow_down:<~t ?| True)
--   in Out "ok" ticks vals
--   where
--     a `implies` b = if a then b else Leaf True

-- delayNegMe :: Declaration TimeT
-- delayNegMe = let
--   ticks = ConstTE 40
--   vals = Leaf (-5)
--   in Out "delayNegMe" ticks vals
