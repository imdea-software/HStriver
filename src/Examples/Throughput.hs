{-# Language RebindableSyntax #-}
module Examples.Throughput where

import HStriver
import Syntax.Booleans
import Syntax.Num
import Declaration.Spec
import Control.Monad.State
import Engine.Table
import Declaration.DecDyn
import Debug.Trace
import qualified Prelude as P
import qualified Data.Map.Strict as Map

main :: Int -> IO ()
main n = let
  --getevs = Map.singleton "clock" [(Ev (t*1, Just$toDyn ())) | t <- [0..n]]
  getevs = undefined
  spec = [out alternator]
  maplist = evalState (loadEvents spec getevs >> dynTableFromSpec spec) initTable
  in print ((fromDynamic :: Dynamic -> Maybe Bool) ((getVal.snd.preLast) maplist))
  where
    preLast [x,_] = x
    preLast (_:l) = preLast l
  -- in print $ map (\(id,y) -> (id, fmap ((fromDynamic :: Dynamic -> Maybe Bool).snd) y)) maplist
  -- in print maplist --map (\(id,y) -> (id, myprint y)) maplist
  -- in print (last maplist) --map (\(id,y) -> (id, myprint y)) maplist

clock :: Stream ()
clock = Input "clock"

clock' :: Stream ()
clock' = Input "clock'"

at50 :: Stream ()
at50 = let
  ticks = ConstTE 5000
  vals = Leaf ()
  in "at50" =: (ticks, vals)

atclock :: Stream ()
atclock = let
  ticks = ticksTE clock
  vals = Leaf ()
  in "atclock" =: (ticks, vals)

--dynTableFromSpec :: Specification -> (DeclarationDyn -> [Event]) -> Stateful [DynEvent]
alternator :: Stream Bool
alternator = let
  ticks = ticksTE clock
  vals = not $ alternator @< (t ?| False)
  in "alternator" =: (ticks, vals)

-- alternator' :: Stream Int
-- alternator' = let
--   ticks = ticksTE alternator
--   --vals = 10 + (traceShowId.unT <$> Tau t) -- alternator @< (t?|0)
--   vals = alternator @< (t?|110)
--   in "alternator'" =: (ticks, vals)
