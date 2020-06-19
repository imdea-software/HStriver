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

ite :: Bool -> a -> a -> a
ite True x _ = x
ite False _ y = y

ifThenElse :: (Typeable a, Typeable cv) => ValExpr cv Bool -> ValExpr cv a -> ValExpr cv a -> ValExpr cv a
ifThenElse b t e = ite <$> b <*> t <*> e

infixr 4 ===
(===) :: (Eq a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a === b = (P.==) <$> a <*> b

infixr 4 /==
(/==) :: (Eq a, Typeable a, Typeable cv) => ValExpr cv a -> ValExpr cv a -> ValExpr cv Bool
a /== b = (P./=) <$> a <*> b
