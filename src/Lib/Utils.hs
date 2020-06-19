module Lib.Utils where
import qualified Prelude as P()
import HStriver

-- foldl :: (b -> a -> b) -> b -> t a -> b
-- hFoldl :: (Streamable a, Streamable b) => Ident -> (b->a->b) -> b -> Stream a -> Stream b
-- hFoldl name combiner neutral dec = name <: dec =: combiner <$> hFoldl name combiner neutral dec:@(-1,Leaf neutral) <*> Now dec

-- constStr :: (Streamable a) => Ident -> a -> Stream a
-- constStr name val = name =: Leaf val

strMap :: (Streamable a, Streamable b) => Ident -> (a->b) -> Stream a -> Stream b
strMap name f dec = let
  ticks = ticksTE dec
  vals = f <$> dec @<~ t
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
