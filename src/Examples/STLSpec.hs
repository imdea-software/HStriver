{-# LANGUAGE RebindableSyntax  #-}
module Examples.STLSpec where
import Syntax.HSPrelude
import Control.Monad.State
import Engine.Table
import Declaration.Spec
import Declaration.Event
import Data.Maybe
import Declaration.Declaration
import Declaration.DecDyn
import Declaration.StaticAnalysis
import Syntax.Booleans
import Syntax.Ord
import System.Random
import Lib.STL as STL

daspec = [out speed, out ok]
testspec = [out testout]

max_speed = 5
ok_speed = 4

inps :: Declaration Double
inps = Input "inps" -- [(fromIntegral x,Just $ toDyn y) | (x,y) <- zip [0..50000000] (randoms $ mkStdGen 1 :: [Double])]

offsets :: Declaration Double
offsets = let
    ticks = ticksTE inps
    vals = let
      sgn = if (offsets @< (t ?| 0))<0 then -1 else 1
      in CV - Leaf 0.5 + Leaf 0.3 * sgn
  in "offsets" =: (ticks, vals)

speed :: Declaration Double
speed = let
    ticks = ticksTE offsets
    vals = (speed @< (t ?| 0)) + CV
  in "speed" =: (ticks, vals)

toofast :: Declaration Bool
toofast = let
    ticks = ticksTE speed
    vals = CV > max_speed
  in "toofast" =: (ticks, vals)

speedok :: Declaration Bool
speedok = let
    ticks = ticksTE speed
    vals = CV <= ok_speed
  in "speedok" =: (ticks, vals)

decel :: Declaration Bool
decel = let
    ticks = ticksTE speed
    vals = CV > (speed @> t)
  in "decel" =: (ticks, vals)

theuntil = STL.until (0,5) decel speedok

slow_down :: Declaration Bool
slow_down = let
  ticks = ticksTE theuntil
  vals = CV
  in "slow_down" =: (ticks, vals)

ok :: Declaration Bool
ok = let
  ticks = ticksTE toofast :+ ticksTE slow_down
  vals = (toofast @<~ (t ?| False)) `implies` (slow_down @<~ (t ?| True))
  in "ok" =: (ticks, vals)
  where
    a `implies` b = if a then b else Leaf True

delayNegMe :: Declaration TimeTDiff
delayNegMe = let
  ticks = ConstTE undefined -- 40 Recover time
  vals = Leaf (-5)
  in "delayNegMe" =: (ticks, vals)

testout :: Declaration ()
testout = let
  ticks = DelayTE Negative delayNegMe
  vals = Leaf ()
  in "testout" =: (ticks, vals)
