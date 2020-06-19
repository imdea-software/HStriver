module Syntax.Num where

import Syntax.HSPrelude
import qualified Prelude as P

import Data.Typeable
import Declaration.Declaration

-- Pretty writing
-- Infix notation for + and *
instance (Typeable a, Typeable cv, Num a) => Num (ValExpr cv a) where
  x + y = (+) <$> x <*> y
  x * y = (*) <$> x <*> y
  fromInteger = Leaf . fromInteger
  abs = App (Leaf abs)
  signum = App (Leaf signum)
  negate = App (Leaf negate)

mod :: (Typeable a,Typeable cv, Integral a) => ValExpr cv a -> ValExpr cv a -> ValExpr cv a
mod a b = P.mod <$> a <*> b

infix 7 /
(/) :: (Typeable a,Typeable cv, Fractional a) => ValExpr cv a -> ValExpr cv a -> ValExpr cv a
a / b = (P./) <$> a <*> b

intdivide :: (Typeable cv) => ValExpr cv Int -> ValExpr cv Int -> ValExpr cv Double
intdivide n m = (fromIntegral <$> n) / (fromIntegral <$> m)

instance (Typeable cv, Typeable a, Fractional a, Eq a) => Fractional (ValExpr cv a) where
  fromRational = Leaf . fromRational
  recip = App (Leaf recip)
