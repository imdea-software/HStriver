module Lib.Utils where
import qualified Prelude as P
import HStriver

-- foldl :: (b -> a -> b) -> b -> t a -> b
-- hFoldl :: (Streamable a, Streamable b) => Ident -> (b->a->b) -> b -> Stream a -> Stream b
-- hFoldl name combiner neutral dec = name <: dec =: combiner <$> hFoldl name combiner neutral dec:@(-1,Leaf neutral) <*> Now dec

-- constStr :: (Streamable a) => Ident -> a -> Stream a
-- constStr name val = name =: Leaf val

strMap :: (Streamable a, Streamable b) => Ident -> (a->b) -> Stream a -> Stream b
strMap name f dec = let
  ticks = ticksTE dec
  vals = f <$> CV
  in
  name <: getId dec =: (ticks,vals)

changePointsOf :: (Streamable a, Eq a) => Stream a -> Stream a
changePointsOf str = let
  ticks = ticksTE str
  prevMVal = Proj (str :<< t)
  mEq = fmap <$> ((==) <$> CV) <*> prevMVal
  update (Ev True) = False
  update _ = True
  vals = (update <$> mEq) :=> CV
  in "changePoints" <: str =: (ticks,vals)

firstEvOf :: (Streamable a) => Stream a -> Stream a
firstEvOf str = let
  ticks = ticksTE str
  _this = firstEvOf str
  vals = ((==NegInfty) <$> Tau (_this :<< TauT)) :=> CV
  in "firstOf" <: str =: (ticks,vals)

joinWith :: (Streamable a) => Ident -> (a->a->a) -> Stream a -> Stream a -> Stream a
joinWith funname fun x y = let
  name = "joinWith_" ++ funname <: x <: y
  ticks = ticksTE x :+ ticksTE y
  vals = operate <$> (Proj (x :<~ t)) <*> (Proj (y :<~t))
  in name =: (ticks,vals)
  where
    operate NegOutside (Ev x) = x
    operate (Ev x) NegOutside = x
    operate (Ev x) (Ev y) = fun x y

strAnd :: Stream Bool -> Stream Bool -> Stream Bool
strAnd = joinWith "/\\" (P.&&)

shift :: (Streamable a) => TimeTDiff -> Stream a -> Stream a
shift n x = "shift" <:n<:x =: (ShiftTE n x, CV)
