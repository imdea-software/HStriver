{-# Language RebindableSyntax #-}
module Lib.RobustSTL where
import Syntax.HSPrelude
import Syntax.Num
import Declaration.DecDyn
import Declaration.Declaration
import Engine.Table
import Declaration.Spec
import Data.Maybe
import Control.Monad.State
import qualified Data.Map.Strict as Map

type StrVal = Int

maxStrVal :: StrVal
maxStrVal = maxBound

minStrVal :: StrVal
minStrVal = minBound

until :: (TimeDiff, TimeDiff) -> Declaration StrVal -> Declaration StrVal -> Declaration StrVal
until (a,b) phi psi = let
  -- name
  name = getId phi ++ "U_" ++ show (a,b) ++ getId psi
  -- phis: The values of phi between (t,t+b), and the one before
  phis = slice (0,b) phi
  -- psis: The values of psi between (t+a,t+b), and the one before
  psis = slice (a,b) psi
  -- ticks: whenever any of these change
  ticks = ticksTE phis :+ ticksTE psis
  -- vals
  phils = stampFst <$> Tau t <*> phis @<~ (t ?| (Nothing, [])) -- maxVal perhaps?
  tplusa = timeDiffPlus <$> Tau t <*> Leaf a
  psils = stampFst <$> tplusa <*> psis @<~ (t ?| (Nothing, [])) -- maxVal perhaps?
  val = fromJust.runSpec <$> (innerspec <$> phils <*> psils)
  in name =: (ticks, val)
  where
    stampFst _ (Nothing, r) = r
    stampFst (T t) (Just v, r) = (t,v):r

-- It also keeps the first value before the interval
slice :: (Typeable a) => (TimeDiff, TimeDiff) -> Declaration a -> Declaration (Maybe a, [(TimeT, a)])
slice (a,b) dec = let
  ticks = ShiftTE (-a) dec :+ ShiftTE (-b) dec
  _this = slice (a,b) dec
  (mold, mnew) = splitCV
  prevPair = _this @< (t?|(Nothing, []))
  newPair = maybe id (\oldv (_,(_:ls)) -> (Just oldv, ls)) <$> mold <*> prevPair
  tplusb = unT <$> (timeDiffPlus <$> Tau t <*> Leaf b)
  vals = maybeAppend <$> tplusb <*> mnew <*> newPair
  in "slice" <: dec <: (a,b) =: (ticks, vals)
  where
    maybeAppend _ Nothing p = p
    maybeAppend t (Just new) (v,ls) = (v,ls ++ [(t, new)])

innerspec :: [(TimeT, StrVal)] -> [(TimeT, StrVal)] -> InnerSpecification StrVal
innerspec xs ys = let
   ysstr = Input "ys"
   xsstr = Input "xs"
   ins = [bind xsstr xs, bind ysstr ys]
   -- xs historical min
   xsminsticks = ticksTE xsstr
   xsminsvals  = min <$> xsminsstr @< (t?|maxStrVal) <*> xsstr @<~ t
   xsminsstr = "xsmins" =: (xsminsticks, xsminsvals)
   -- minimum of ys and xs historical min
   theMinsTicks = ticksTE xsminsstr :+ ticksTE ysstr
   theMinsVal = min <$> ysstr @<~ (t?|maxStrVal) <*> xsminsstr @<~ (t?|maxStrVal)
   theMins = "theMins" =: (theMinsTicks, theMinsVal)
   theMaxMinTicks = ticksTE theMins
   theMaxMinVals = max <$> theMaxMin @< (t?|minStrVal) <*> theMins @<~ t
   theMaxMin = "theMaxMins" =: (theMaxMinTicks, theMaxMinVals)
   -- stopstream (empty, will return last)
   empty = "empty" =: (ConstTE 0, notick)
   --
   in IS ins theMaxMin empty
