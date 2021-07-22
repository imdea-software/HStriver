{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
module HStriver
  (Declaration.DecDyn.Specification,
  Declaration.DecDyn.out,
  module Declaration.Declaration,
  Declaration.Event.Time(..),
  Declaration.Event.MaybeOutside(..),
  module Syntax.HSPrelude,
  module HStriver,
  Declaration.VarArgInput.input)
where
import Declaration.Declaration
import Declaration.Event
import Declaration.DecDyn
import Syntax.HSPrelude
import Engine.Table
import Declaration.VarArgInput

class ToolLift r f where
  toolLift :: f -> r

instance {-# OVERLAPS #-} (a ~ a', Streamable a, Streamable cv) => ToolLift (ValExpr cv a') a where
  toolLift = magic0

instance {-# OVERLAPS #-} (a ~ a', b~b', Streamable a, Streamable b, Streamable cv) => ToolLift (ValExpr cv a' -> ValExpr cv b') (a->b) where
  toolLift = magic1

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', Streamable a, Streamable b, Streamable c, Streamable cv) => ToolLift (ValExpr cv a' -> ValExpr cv b' -> ValExpr cv c') (a->b->c) where
  toolLift = magic2

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', d~d', Streamable a, Streamable b, Streamable c, Streamable d, Streamable cv) => ToolLift (ValExpr cv a' -> ValExpr cv b' -> ValExpr cv c' -> ValExpr cv d') (a->b->c->d) where
  toolLift = magic3

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', d~d', a0~a0', a1~a1', a2~a2', a3~a3', Streamable a, Streamable b, Streamable c, Streamable d, Streamable a0, Streamable a1, Streamable a2, Streamable a3, Streamable cv) => ToolLift (ValExpr cv a' -> ValExpr cv b' -> ValExpr cv c' -> ValExpr cv d' -> ValExpr cv a0 -> ValExpr cv a1 -> ValExpr cv a2 -> ValExpr cv a3) (a->b->c->d->a0->a1->a2->a3) where
  toolLift = magic7

magic0 f = Leaf f
magic1 f x = f Declaration.Declaration.<$> x
magic2 f x y = f Declaration.Declaration.<$> x Declaration.Declaration.<*> y
magic3 f x y z = f Declaration.Declaration.<$> x Declaration.Declaration.<*> y Declaration.Declaration.<*> z
magic4 f x y z a0 = f Declaration.Declaration.<$> x Declaration.Declaration.<*> y Declaration.Declaration.<*> z Declaration.Declaration.<*> a0
magic5 f x y z a0 a1 = f Declaration.Declaration.<$> x Declaration.Declaration.<*> y Declaration.Declaration.<*> z Declaration.Declaration.<*> a0 Declaration.Declaration.<*> a1
magic6 f x y z a0 a1 a2= f Declaration.Declaration.<$> x Declaration.Declaration.<*> y Declaration.Declaration.<*> z Declaration.Declaration.<*> a0 Declaration.Declaration.<*> a1 Declaration.Declaration.<*> a2
magic7 f x y z a0 a1 a2 a3 = f Declaration.Declaration.<$> x Declaration.Declaration.<*> y Declaration.Declaration.<*> z Declaration.Declaration.<*> a0 Declaration.Declaration.<*> a1 Declaration.Declaration.<*> a2 Declaration.Declaration.<*> a3

-- Auto cv
--

split0CV :: (Typeable a) => ValExpr a a
split0CV = CV
split1CV :: (Typeable a, Typeable b) => (ValExpr (a,b) a, ValExpr (a,b) b)
split1CV = splitCV

-- New tau

timeOf :: Typeable cv => TauExpr x -> ValExpr cv Time
timeOf = Tau
