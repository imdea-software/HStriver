{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE GADTs#-}
module Declaration.DecDyn where

import Declaration.Declaration
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 (unpack)

type DecDyn = (DeclarationDyn, Dynamic -> Value)

type Specification = [DecDyn]

data Format = JSON | CSV deriving Eq

data TickExprDyn =
    DConstTE TimeT
  | DUnion TickExprDyn TickExprDyn Dynamic
  | DShiftTE TimeTDiff DeclarationDyn
  | DDelayTE DelayDir DeclarationDyn

data ValExprDyn =
    DLeaf Dynamic
  | DCV
  | DOrNoTick ValExprDyn ValExprDyn
  | DApp ValExprDyn ValExprDyn
  | DTau TauExprDyn
  | DProj TauExprDyn (Event -> Dynamic)

data TauExprDyn =
    DTauT
  | DPrev DeclarationDyn TauExprDyn
  | DPrevEq DeclarationDyn TauExprDyn
  | DSucc DeclarationDyn TauExprDyn
  | DSuccEq DeclarationDyn TauExprDyn
  | DBoundedSucc TimeTDiff DeclarationDyn TauExprDyn

data DeclarationDyn =
    DInp Ident (Value->Dynamic)
  | DOut Ident TickExprDyn ValExprDyn

out :: forall a.(Typeable a, ToJSON a) => Declaration a -> DecDyn
out = dec2Dyn

dgetId (DInp x _) = x
dgetId (DOut x _ _) = x

dec2Dyn :: forall a. (Typeable a, ToJSON a) => Declaration a -> DecDyn
dec2Dyn dec = let
  thea = (fromMaybe $ error "No dynamic").(fromDynamic :: Dynamic -> Maybe a) in
  (dec2Dyn' dec, toJSON.thea)

dec2Dyn' :: forall a. Declaration a -> DeclarationDyn
dec2Dyn' (Input id) = let
  valtodyn = toDyn.(fromJust.(parseMaybe parseJSON) :: Value -> a)
  in
  DInp id valtodyn
dec2Dyn' (Output id tickexp valexp) = DOut id (te2Dyn tickexp) (vale2DynTop valexp)

te2Dyn :: TickExpr a -> TickExprDyn
te2Dyn (ConstTE t) = DConstTE t
te2Dyn ((te1 :: TickExpr cv0) :+ (te2 :: TickExpr cv1)) = DUnion (te2Dyn te1) (te2Dyn te2) (toDyn $ joinUsing (fromDynamic :: Dynamic -> Maybe cv0, fromDynamic :: Dynamic -> Maybe cv1))
te2Dyn (ShiftTE t dec) = DShiftTE t (dec2Dyn' dec)
te2Dyn (DelayTE t dec) = DDelayTE t (dec2Dyn' dec)

joinUsing :: (Dynamic->Maybe a, Dynamic->Maybe b) -> (Maybe Dynamic, Maybe Dynamic) -> (Maybe a, Maybe b)
joinUsing (f1,f2) (ma, mb) = let
  a = ma >>= f1
  b = mb >>= f2
  in (a,b)

tau2Dyn :: forall a. TauExpr a -> TauExprDyn
tau2Dyn TauT = DTauT
tau2Dyn (x :<< y) = DPrev (dec2Dyn' x) (tau2Dyn y)
tau2Dyn (x :<~ y) = DPrevEq (dec2Dyn' x) (tau2Dyn y)
tau2Dyn (x :>> y) = DSucc (dec2Dyn' x) (tau2Dyn y)
tau2Dyn (x :>~ y) = DSuccEq (dec2Dyn' x) (tau2Dyn y)
tau2Dyn (BoundedSucc b x y) = DBoundedSucc b (dec2Dyn' x) (tau2Dyn y)

vale2DynTop :: ValExpr cv a -> ValExprDyn
vale2DynTop (x :=> y) = let
    dx = vale2Dyn x
    dy = vale2Dyn y
  in DOrNoTick dx dy
vale2DynTop x = vale2Dyn x

vale2Dyn :: ValExpr cv a -> ValExprDyn
vale2Dyn (Leaf x) = DLeaf (toDyn x)
vale2Dyn CV = DCV
vale2Dyn (App x y) = let
    dx = vale2Dyn x
    dy = vale2Dyn y
  in DApp dx dy
vale2Dyn (Tau x) = DTau (tau2Dyn x)
vale2Dyn (Proj (x :: TauExpr a)) = DProj (tau2Dyn x) (\x -> (toDyn $! (maybegetval x)))
  where
  maybegetval (Ev (_, Just v)) =
    let Just y = fromDynamic v :: Maybe a
    in
    y `seq` Ev (((fromMaybe $ error "No dynamic").fromDynamic) v)
  maybegetval NegOutside = (NegOutside :: MaybeOutside a)
  maybegetval PosOutside = (PosOutside :: MaybeOutside a)
vale2Dyn (x :=> y) = error "Nested usage of :=> is not allowed"
