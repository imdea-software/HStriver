{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE UndecidableInstances#-}
module Syntax.Booleans where

import Syntax.HSPrelude
import qualified Prelude as P

import Declaration.Declaration
import Data.Typeable

infixr 2 ||
(||) :: (Typeable cv) => ValExpr cv Bool -> ValExpr cv Bool -> ValExpr cv Bool
a || b = (P.||) <$> a <*> b

infixr 3 &&
(&&) :: (Typeable cv) => ValExpr cv Bool -> ValExpr cv Bool -> ValExpr cv Bool
a && b = (P.&&) <$> a <*> b

not :: (Typeable cv) => ValExpr cv Bool -> ValExpr cv Bool
not a = P.not <$> a

infixr 2 `implies`
implies :: (Typeable cv) => ValExpr cv Bool -> ValExpr cv Bool -> ValExpr cv Bool
implies a b = not a || b

-- ite :: Bool -> a -> a -> a
-- ite True x _ = x
-- ite False _ y = y

ifThenElse :: (Typeable a, Typeable cv) => ValExpr cv Bool -> ValExpr cv a -> ValExpr cv a -> ValExpr cv a
ifThenElse = ITE

infixr 4 ===
(===) :: (Eq a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a === b = (P.==) <$> a <*> b

infixr 4 /==
(/==) :: (Eq a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a /== b = (P./=) <$> a <*> b

neg :: Declaration Bool -> Declaration Bool
neg x = "not" <: x =: (ticksTE x, not CV)

disj :: Declaration Bool -> Declaration Bool -> Declaration Bool
disj x y = let
    name = "or" <: x <: y
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| False) || y @<~ (t ?| False)
  in name =: (ticks, vals)

conj :: Declaration Bool -> Declaration Bool -> Declaration Bool
conj x y = let
    name = "and" <: x <: y
    ticks = ticksTE x :+ ticksTE y
    vals = x @<~ (t ?| True) && y @<~ (t ?| True)
  in name =: (ticks, vals)

(?=>) :: (Typeable a, Typeable cv) => ValExpr cv Bool -> ValExpr cv a -> ValExpr cv a
x ?=> y = ifThenElse x y notick
