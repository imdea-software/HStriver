{-# LANGUAGE RebindableSyntax  #-}
module Lib.STL (true, mu, neg, disj, until) where
import Syntax.HSPrelude
import Declaration.Declaration
import Syntax.Ord
import Syntax.Num
import Syntax.Booleans
import Data.Typeable
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

disj :: Ident -> Declaration Bool -> Declaration Bool -> Declaration Bool
disj name x y = let
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| False) || y @<~ (t ?| False)
  in name =: (ticks, vals)

shift :: Typeable a => Ident -> TimeTDiff -> Declaration a -> Declaration a
shift name t x = let
    ticks = ShiftTE t x
    vals = CV
  in name =: (ticks, vals)

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
        min_yT = if (shift (name ++ "::shift_y") (-a) y) @<~ (t ?| False) then Tau t else Tau ((shift (name ++ "::shift_yT") (-a) (filterStream (name++"::filter_yT") id y) >>| (b-a)) t)
        min_xF = if not (x @<~ (t ?| False)) then Tau t else Tau ((filterStream (name++"::filter_xF") P.not x >>| b) t) in
      min_yT `plus` a <= Tau t `plus` b && min_yT `plus` a <= min_xF
  in name =: (ticks, vals)
  where
    plus ve1 daT = innplus <$> ve1 <*> Leaf daT
    innplus = timeDiffPlus
