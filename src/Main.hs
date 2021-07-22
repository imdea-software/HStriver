{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import GHC.Generics
import Data.Aeson
import InFromFile
import System.IO
import System.Environment
import HStriver
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import Declaration.DecDyn
import Declaration.Spec
import qualified Prelude as P
import System.Random
import Data.Maybe
import Lib.STL

main :: IO ()
main = (parseArgs P.<$> getArgs) >>= mapM_ putStrLn

parseArgs :: [String] -> [String]
parseArgs [densitystr, winsizestr, tlenstr] = let
  density = read densitystr
  tlen = read tlenstr
  winsize = read winsizestr
  ins = makeins tlen (1 P./ density)
  outevsold = runSpecMocked [out $ ok winsize] ins
  outevs = runSpecMocked [out speed] ins
  in outevs

makeins :: Int -> Double -> [(Ident, [Event])]
makeins tlen dt = let
  ts1 = map ((* dt).fromIntegral) [0..]
  -- vals1 = map (Just. toDyn::Double -> Maybe Dynamic) $ (randomRs (0,1) (mkStdGen 1337))
  vals1 = map (Just. toDyn::Double -> Maybe Dynamic) $ take 20000 (randomRs (0,1) (mkStdGen 1337)) ++ repeat 1
  s1 = map Ev $ take tlen (zip ts1 vals1)
  in [("rand", s1)]

max_speed = 3
ok_speed = 1

-- -=-=-=-=-=-=-=-=-=-=-=-=-=

rand :: Stream Double
rand = input "rand"

sgn :: Stream Double
sgn = "sgn" =: let
  ticks = ticksTE nxtoff
  val = let spd = speed@<~t in
    if spd > 10 then -1 else if spd < -1.5 then 1 else if CV < 0 then -1 else 1
  in (ticks,val)

nxtoff :: Stream Double
nxtoff = "nxtoff" =: let
  ticks = ticksTE rand
  val = (sgn @< (t?|0)) * 0.3 + CV - 0.5
  in (ticks,val)

speed :: Stream Double
speed = "speed" =: let
  ticks = ticksTE nxtoff
  val = speed @<(t?|0) + CV
  in (ticks,val)

toofast :: Stream Bool
toofast = "toofast" =: let
  ticks = ticksTE speed
  val = CV > max_speed
  in (ticks,val)

okspeed :: Stream Bool
okspeed = "okspeed" =: let
  ticks = ticksTE speed
  val = CV < ok_speed
  in (ticks,val)

decel :: Stream Bool
decel = "decel" =: let
  ticks = ticksTE speed
  val = CV > speed @> (t?|0)
  in (ticks,val)

slow_down :: Double -> Stream Bool
-- slow_down winsize = ineffuntil (0,winsize) decel okspeed
slow_down winsize = until (0,winsize) decel okspeed

ok :: Double -> Stream Bool
ok winsize = "ok" =: let
  sld = slow_down winsize
  ticks = ticksTE toofast :+ ticksTE sld
  val = (toofast @<~ (t?|False)) `implies` (sld @<~ (t?|True))
  in (ticks,val)
