{-# LANGUAGE RebindableSyntax  #-}
module Lib.STL (mu, neg, until, ineffuntil, always, eventually) where
import Syntax.HSPrelude hiding (and, or)
import Declaration.Declaration
import Syntax.Ord
import Syntax.Num()
import Lib.Utils
import Syntax.Booleans
import Data.Maybe
import qualified Prelude as P

mu :: (Ord b, Num b, Typeable a, Typeable b) => Ident -> Declaration a -> (a->b) -> Declaration Bool
mu name x f = let
    ticks = ticksTE x
    vals = (f <$> CV) > 0
  in name =: (ticks, vals)

until :: (TimeDiff, TimeDiff) -> Declaration Bool -> Declaration Bool -> Declaration Bool
until (a,b) x y = let
    name = getId x ++ "U_" ++ show (a,b) ++ getId y
    ticks = ShiftTE (-a) y :+ ShiftTE (-b) y :+ ShiftTE (-b) x :+ ticksTE x
    vals = let
      yT = Lib.Utils.filter "id" id y
      min_yT = if Lib.Utils.shift (-a) y @<~ (t ?| False) then Tau t else Tau ((Lib.Utils.shift (-a) yT >>| (b-a)) t)
      xF = Lib.Utils.filter "not" P.not x
      min_xF = if not (x @<~ (t ?| False)) then Tau t else Tau ((xF >>| b) t)
      plus ve1 daT = timeDiffPlus <$> ve1 <*> Leaf daT
      in
      min_yT `plus` a <= Tau t `plus` b && min_yT `plus` a <= min_xF
  in name =: (ticks, vals)

ineffuntil :: (TimeDiff, TimeDiff) -> Declaration Bool -> Declaration Bool -> Declaration Bool
ineffuntil (a,b) x y = let
    name = getId x ++ "Uineff_" ++ show (a,b) ++ getId y
    ticks = ShiftTE (-a) y :+ ShiftTE (-b) y :+ ShiftTE (-b) x :+ ticksTE x
    vals = let
      yT = Lib.Utils.filter "id" id y
      min_yT = if Lib.Utils.shift (-a) y @<~ (t ?| False) then Tau t else Tau (Lib.Utils.shift (-a) yT :>> t)
      xF = Lib.Utils.filter "not" P.not x
      min_xF = if not (x @<~ (t ?| False)) then Tau t else Tau (xF :>> t)
      plus ve1 daT = timeDiffPlus <$> ve1 <*> Leaf daT
      in
      min_yT `plus` a <= Tau t `plus` b && min_yT `plus` a <= min_xF
  in name =: (ticks, vals)

alwaysaux :: TimeDiff -> Declaration Bool -> Declaration TimeDiff
alwaysaux n x = let
  name = "alwaysaux" <: n <: x
  ticks = ticksTE x
  vals = not CV ?=> if not (x @> (t?|False)) then (-1) else (tDiff <$> (tDiffAdd <$> (unT <$> Tau (x:>>t)) <*> Leaf n) <*> now)
  in name =: (ticks,vals)

statealways :: TimeDiff -> Declaration Bool -> Declaration Bool
statealways n x = let
  name = "statealways"<:n<:x
  aaux = shift (-n) (alwaysaux n x)
  ticks = DelayTE Positive aaux :+ ticksTE aaux
  (_,mfalse) = splitCV
  vals = not (isJust <$> mfalse)
  in name =: (ticks,vals)

always :: (TimeDiff, TimeDiff) -> Declaration Bool -> Declaration Bool
always (a,b) x = let
  _this = always (a,b) x
  name = "always" <: (a,b) <: x
  ticks = ShiftTE (-a) (statealways (b-a) x) :+ ConstTE 0
  (mv,_) = splitCV
  daval = fromMaybe <$> (_this @< (t?|True)) <*> mv
  in name =: (ticks, daval)

eventually :: (TimeDiff, TimeDiff) -> Declaration Bool -> Declaration Bool
eventually (a,b) x = let
  name = "eventually" <: (a,b) <: x
  notx = strMap "not" P.not x
  alwaysnotx = always (a,b) notx
  in name ==: strMap "not" P.not alwaysnotx
