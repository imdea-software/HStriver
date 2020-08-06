{-# LANGUAGE RebindableSyntax  #-}
module Lib.STL (true, mu, neg, or, until, always, eventually, and, implies) where
import Syntax.HSPrelude hiding (and, or)
import Declaration.Declaration
import Syntax.Ord
import Syntax.Num()
import Lib.Utils
import Syntax.Booleans
import Data.Maybe
import qualified Prelude as P

true :: Ident -> Declaration Bool
true name = let
    ticks = ConstTE undefined -- 0 Recover time as int
    vals = Leaf True
  in name =: (ticks, vals)

mu :: (Ord b, Num b, Typeable a, Typeable b) => Ident -> Declaration a -> (a->b) -> Declaration Bool
mu name x f = let
    ticks = ticksTE x
    vals = (f <$> CV) > 0
  in name =: (ticks, vals)

neg :: Ident -> Declaration Bool -> Declaration Bool
neg name x = let
    ticks = ticksTE x
    vals = not CV
  in name =: (ticks, vals)

or :: Declaration Bool -> Declaration Bool -> Declaration Bool
or x y = let
    name = "or" <: x <: y
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| False) || y @<~ (t ?| False)
  in name =: (ticks, vals)

and :: Declaration Bool -> Declaration Bool -> Declaration Bool
and x y = let
    name = "and" <: x <: y
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| True) && y @<~ (t ?| True)
  in name =: (ticks, vals)

implies :: Declaration Bool -> Declaration Bool -> Declaration Bool
implies x y = let
    name = "implies" <: x <: y
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| False) <= y @<~ (t ?| True)
  in name =: (ticks, vals)

shifty :: Typeable a => Ident -> TimeTDiff -> Declaration a -> Declaration a
shifty name t x = name ==: shift t x

filterStream :: (Typeable a) => Ident -> (a->Bool) -> Declaration a -> Declaration a
filterStream name f x = let
    ticks = ticksTE x
    vals = (f <$> CV) :=> CV
  in name =: (ticks, vals)

until :: (TimeTDiff, TimeTDiff) -> Declaration Bool -> Declaration Bool -> Declaration Bool
until (a,b) x y = let
    name = getId x ++ "U_" ++ show (a,b) ++ getId y
    ticks = ShiftTE (-a) y :+ ShiftTE (-b) y :+ ShiftTE (-b) x :+ ticksTE x
    vals = let
        min_yT = if (shifty (name ++ "::shift_y") (-a) y) @<~ (t ?| False) then Tau t else Tau ((shifty (name ++ "::shift_yT") (-a) (filterStream (name++"::filter_yT") id y) >>| (b-a)) t)
        min_xF = if not (x @<~ (t ?| False)) then Tau t else Tau ((filterStream (name++"::filter_xF") P.not x >>| b) t) in
      min_yT `plus` a <= Tau t `plus` b && min_yT `plus` a <= min_xF
  in name =: (ticks, vals)
  where
    plus ve1 daT = innplus <$> ve1 <*> Leaf daT
    innplus = timeDiffPlus

alwaysaux :: TimeTDiff -> Declaration Bool -> Declaration TimeTDiff
alwaysaux n x = let
  name = "alwaysaux" <: n <: x
  ticks = ticksTE x
  vals = not CV :=> if not (x @> (t?|False)) then (-1) else Leaf n + (unT <$> Tau (x:>>t)) - now
  in name =: (ticks,vals)

statealways :: TimeTDiff -> Declaration Bool -> Declaration Bool
statealways n x = let
  name = "statealways"<:n<:x
  aaux = shift (-n) (alwaysaux n x)
  ticks = DelayTE Positive aaux :+ ticksTE aaux
  (_,mfalse) = splitCV
  vals = not (isJust <$> mfalse)
  in name =: (ticks,vals)

always :: (TimeTDiff, TimeTDiff) -> Declaration Bool -> Declaration Bool
always (a,b) x = let
  _this = always (a,b) x
  name = "always" <: (a,b) <: x
  ticks = ShiftTE (-a) (statealways (b-a) x) :+ ConstTE 0
  (mv,_) = splitCV
  daval = fromMaybe <$> (_this @< (t?|True)) <*> mv
  in name =: (ticks, daval)

eventually :: (TimeTDiff, TimeTDiff) -> Declaration Bool -> Declaration Bool
eventually (a,b) x = let
  name = "eventually" <: (a,b) <: x
  notx = strMap "not" P.not x
  alwaysnotx = always (a,b) notx
  in name ==: strMap "not" P.not alwaysnotx
