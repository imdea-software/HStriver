module Syntax.Ord where

import Syntax.HSPrelude
import qualified Prelude as P

import Declaration.Declaration
import Data.Typeable

infix 4 <
(<) :: (Ord a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a < b = (P.<) <$> a <*> b

infix 4 >
(>) :: (Ord a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a > b = (P.>) <$> a <*> b

infix 4 <=
(<=) :: (Ord a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a <= b = (P.<=) <$> a <*> b

infix 4 >=
(>=) :: (Ord a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a >= b = (P.>=) <$> a <*> b
