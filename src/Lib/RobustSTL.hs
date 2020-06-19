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

-- The stream will start producing the right values when both input streams are
-- initialized (until then, the value will probably be - \infty)
until :: (TimeTDiff, TimeTDiff) -> Declaration StrVal -> Declaration StrVal -> Declaration StrVal
until (a,b) phi psi = let
  -- name
  name = getId phi ++ "U_" ++ show (a,b) ++ getId psi
  -- phis: The values of phi between (t,t+b), and the one before
  phis = valsInInterval name (0,b) phi
  -- psis: The values of psi between (t+a,t+b), and the one before
  psis = valsInInterval name (a,b) psi
  -- ticks: whenever any of these change
  ticks = ticksTE phis :+ ticksTE psis
  -- vals
  phils = stampFst <$> Tau t <*> phis @<~ (t ?| [NegOutside]) -- maxVal perhaps?
  tplusa = timeDiffPlus <$> Tau t <*> Leaf a
  psils = stampFst <$> tplusa <*> psis @<~ (t ?| [NegOutside]) -- maxVal perhaps?
  val = getMaxMinUntil <$> phils <*> psils
  in name =: (ticks, val)
  where
    stampFst _ (NegOutside:r) = NegOutside:r
    stampFst (T t) (Ev (_,v):r) = Ev (t,v):r

-- It also keeps the first value before the interval
valsInInterval :: Ident -> (TimeTDiff, TimeTDiff) -> Declaration StrVal -> Declaration [MaybeOutside (TimeT, StrVal)]
valsInInterval name (a,b) dec = let
  ticks = ShiftTE (-a) dec :+ ShiftTE (-b) dec
  _this = valsInInterval name (a,b) dec
  (mold, mnew) = splitCV
  prevList = _this @< (t?|[NegOutside])
  newList = maybe id (const tail) <$> mold <*> prevList
  tplusb = unT <$> (timeDiffPlus <$> Tau t <*> Leaf b)
  vals = maybeAppend <$> tplusb <*> mnew <*> newList
  in (name ++ "::"++ getId dec) =: (ticks, vals)
  where
    maybeAppend _ Nothing l = l
    maybeAppend t (Just new) ls = ls ++ [Ev (t, new)]

getMaxMinUntil :: [MaybeOutside (TimeT, StrVal)] -> [MaybeOutside (TimeT, StrVal)] -> StrVal
getMaxMinUntil xs ys = let
   ysstr = Input "ys"
   xsstr = Input "xs"
   mapin = Map.fromList [("xs", [Ev (t, Just $ toDyn x) | Ev (t,x) <- xs]),("ys", [Ev (t, Just $ toDyn x) | Ev (t,x) <- ys])]
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
   spec = [out theMaxMin]
   dynevs = evalState (loadEvents spec mapin >> dynTableFromSpec spec) initTable
   in last $ catMaybesEv $ map (\(_,v) -> fmap (\(_,d) -> (fromJust.fromDynamic) d) v) dynevs
   where
   catMaybesEv xs = [x | Ev x <- xs]

-- until2 :: (TimeT, TimeT) -> Declaration StrVal -> Declaration StrVal -> Declaration StrVal
-- until2 (a,b) phi psi = let
--   -- name
--   name = getId phi ++ "U_" ++ show (a,b) ++ getId psi
--   -- ticks
--   phis = valsInInterval name (0,b) phi
--   psis = valsInInterval name (a,b) psi
--   ticks = ticksTE phis :+ ticksTE psis
--   -- vals
--   phils = stampFst <$> Tau t <*> phis :<~ t ?| [(NegInfty, maxStrVal)]
--   psils = stampFst <$> (plus <$> Tau t <*> Leaf a) <*> psis :<~ t ?| [(NegInfty, maxStrVal)]
--   vals = maximum <$> (mergeByTS <$> phils <*> psils)
--   in Out name ticks vals
--   where
--     stampFst t ((_,v):r) = (t,v):r

-- true :: Ident -> Declaration Bool
-- true name = let
--     ticks = ConstTE 0
--     vals = Leaf True
--   in Out name ticks vals

-- mu :: (Ord b, Num b, Typeable a, Typeable b) => Ident -> Declaration a -> (a->b) -> Declaration Bool
-- mu name x f = let
--     ticks = ticksTE x
--     vals = (f <$> CV) > 0
--   in Out name ticks vals

-- neg :: Ident -> Declaration Bool -> Declaration Bool
-- neg name x = let
--     ticks = ticksTE x
--     vals = not CV
--   in Out name ticks vals

-- disj :: Ident -> Declaration Bool -> Declaration Bool -> Declaration Bool
-- disj name x y = let
--     ticks = ticksTE x :+ ticksTE y
--     vals = x :<~ t ?| False || y :<~ t ?| False
--   in Out name ticks vals

-- shift :: Typeable a => Ident -> TimeT -> Declaration a -> Declaration a
-- shift name t x = let
--     ticks = ShiftTE t x
--     vals = CV
--   in Out name ticks vals

-- filterStream :: (Typeable a) => Ident -> (a->Bool) -> Declaration a -> Declaration a
-- filterStream name f x = let
--     ticks = ticksTE x
--     vals = (f <$> CV) :=> CV
--   in Out name ticks vals

-- until :: (TimeT, TimeT) -> Declaration Bool -> Declaration Bool -> Declaration Bool
-- until (a,b) x y = let
--     name = getId x ++ "U_" ++ show (a,b) ++ getId y
--     ticks = ShiftTE (-a) y :+ ShiftTE (-b) y :+ ShiftTE (-b) x :+ ticksTE x
--     vals = let
--         min_yT = if (shift (name ++ "::shift_y") (-a) y :<~ t) ?| False then Tau t else Tau ((shift (name ++ "::shift_yT") (-a) (filterStream (name++"::filter_yT") id y) >>| (b-a)) t)
--         min_xF = if not (x:<~t ?| False) then Tau t else Tau ((filterStream (name++"::filter_xF") P.not x >>| b) t) in
--       min_yT `plus` a <= Tau t `plus` b && min_yT `plus` a <= min_xF
--   in Out name ticks vals
--   where
--     plus ve1 daT = innplus <$> ve1 <*> Leaf daT
--     innplus (T x) y = T (x+y)
--     innplus x _ = x
