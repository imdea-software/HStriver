{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Declaration.Declaration (module Declaration.Declaration, module Declaration.Event, Declaration.Event.DelayDir(..)) where
import Declaration.Event
import Data.Aeson
import Data.Maybe
import Data.Tuple.Extra

type Streamable = Typeable
type Stream = Declaration

data TickExpr cv where
  ConstTE :: TimeT -> TickExpr ()
  (:+) :: (Typeable cv0, Typeable cv1) => TickExpr cv0 -> TickExpr cv1 -> TickExpr (Maybe cv0, Maybe cv1)
  ShiftTE :: TimeTDiff -> Declaration cv -> TickExpr cv
  DelayTE :: DelayDir -> Declaration TimeTDiff -> TickExpr ()

data ValExpr cv a where
  Leaf :: (Typeable a, Typeable cv) => a -> ValExpr cv a
  CV :: Typeable cv => ValExpr cv cv
  App :: (Typeable (b->a), Typeable b, Typeable a, Typeable cv) => ValExpr cv (b->a) -> ValExpr cv b -> ValExpr cv a
  -- "Or no tick". This is the only way of returning a notick
  (:=>) :: (Typeable a, Typeable cv) => ValExpr cv Bool -> ValExpr cv a -> ValExpr cv a
  Tau :: Typeable cv => TauExpr x -> ValExpr cv Time
  Proj :: (Typeable cv, Typeable x) => TauExpr x -> ValExpr cv (MaybeOutside x)

data TauExpr a where
  TauT :: TauExpr a
  (:<<) :: Declaration a -> TauExpr b -> TauExpr a
  (:<~) :: Declaration a -> TauExpr b -> TauExpr a
  (:>>) :: Declaration a -> TauExpr b -> TauExpr a
  (:>~) :: Declaration a -> TauExpr b -> TauExpr a
  BoundedSucc :: TimeTDiff -> Declaration a -> TauExpr b -> TauExpr a

data Declaration a where
  Input :: (FromJSON a,Typeable a) => Ident -> Declaration a
  Output :: (Typeable a, Typeable cv) => Ident -> TickExpr cv -> ValExpr cv a -> Declaration a

getId :: Declaration a -> Ident
getId (Input id) = id
getId (Output id _ _) = id

-- Utils

infix 1 =:
(=:) :: (Streamable a, Streamable cv) => Ident -> (TickExpr cv, ValExpr cv a) -> Declaration a
s =: (te,ve) = Output s te ve

infix 1 ==:
(==:) :: (Streamable a) => Ident -> Declaration a -> Declaration a
s ==: Output _ te ve = s =: (te, ve)
s ==: x@(Input _) = s =: (ticksTE x, CV)

infixl 4 <$>
(<$>) :: (Typeable a, Typeable b, Typeable cv) => (a -> b) -> ValExpr cv a -> ValExpr cv b
f <$> e = App (Leaf f) e

infixl 4 <*>
(<*>) :: (Typeable a, Typeable b, Typeable cv) => ValExpr cv (a -> b) -> ValExpr cv a -> ValExpr cv b
e0 <*> e1 = App e0 e1

instance Show (Declaration a) where
  show = getId

infixl 2 <:
(<:) :: Show a => Ident -> a -> Ident
ident <: decName = ident ++ ('<':show decName ++ ">")

t :: TauExpr a
t = TauT

now :: (Streamable cv) => ValExpr cv TimeT
now = unT Declaration.Declaration.<$> Tau t

data MyAcc = Prev | PrevEq | Succ | SuccEq

(@<) :: (Atable f, Typeable cv, Typeable a) => Declaration a -> f a -> ValExpr cv a
a @< x = fromAtable x Prev a

(@<~) :: (Atable f, Typeable cv, Typeable a) => Declaration a -> f a -> ValExpr cv a
a @<~ x = fromAtable x PrevEq a

(@>) :: (Atable f, Typeable cv, Typeable a) => Declaration a -> f a -> ValExpr cv a
a @> x = fromAtable x Succ a

(@~>) ::  (Atable f, Typeable cv, Typeable a) => Declaration a -> f a -> ValExpr cv a
a @~> x = fromAtable x SuccEq a

class Atable f where
  fromAtable :: (Typeable cv, Typeable a) => f a -> MyAcc -> Declaration a -> ValExpr cv a

data MyAtable a where
  OrDflt :: (Typeable a) => TauExpr b -> a -> MyAtable a

(?|) :: (Typeable a) => TauExpr b -> a -> MyAtable a
taue ?| dfl = OrDflt taue dfl

instance Atable MyAtable where
  fromAtable (OrDflt taue dfl) constructor dec = projDefault (getConstructor constructor dec taue,dfl)

instance Atable TauExpr where
  fromAtable taue constructor dec = projGet (getConstructor constructor dec taue)

getConstructor Prev = (:<<)
getConstructor PrevEq = (:<~)
getConstructor Succ = (:>>)
getConstructor SuccEq = (:>~)

-- TODO define precedence

(>>|) :: Declaration a -> TimeTDiff -> TauExpr b -> TauExpr a
(x >>| b) y = BoundedSucc b x y

projGet texp = unEv Declaration.Declaration.<$> Proj texp
  where
    unEv (Ev x) = x

projDefault (texp, d) = daProj Declaration.Declaration.<$> Proj texp
  where
    daProj (Ev x) = x
    daProj _ = d

-- (?|) :: (Typeable cv, Typeable a) => TauExpr a -> a -> ValExpr cv a
-- texp ?| x = getValDefault Declaration.Declaration.<$> Proj texp
--   where
--     getValDefault (Ev v) = v
--     getValDefault _ = x

(?||) :: (Typeable cv, Typeable a) => TauExpr a -> ValExpr cv a -> ValExpr cv a
texp ?|| x = getValDefault Declaration.Declaration.<$> Proj texp Declaration.Declaration.<*> x
  where
    getValDefault (Ev v) _ = v
    getValDefault _ y = y

notick :: (Typeable a, Typeable cv) => ValExpr cv a
notick = Leaf False :=> Leaf undefined

maybenotick :: (Typeable a, Typeable cv) => ValExpr cv (Maybe a) -> ValExpr cv a
maybenotick e = (isJust Declaration.Declaration.<$> e) :=> (fromJust Declaration.Declaration.<$> e)

ticksTE :: Typeable cv => Declaration cv -> TickExpr cv
ticksTE = ShiftTE 0

splitCV :: (Typeable a, Typeable b) => (ValExpr (a,b) a, ValExpr (a,b) b)
splitCV = (fst Declaration.Declaration.<$> CV, snd Declaration.Declaration.<$> CV)

comb3CV :: (Typeable a, Typeable b, Typeable c) => ValExpr (Maybe (Maybe a, Maybe b),Maybe c) (Maybe a, Maybe b, Maybe c)
comb3CV = f Declaration.Declaration.<$> CV
  where f (x,y) = (maybe Nothing fst x, maybe Nothing snd x, y)

split3CV :: (Typeable a, Typeable b, Typeable c) => (ValExpr (Maybe (Maybe a, Maybe b),Maybe c) (Maybe a), ValExpr (Maybe (Maybe a, Maybe b),Maybe c) (Maybe b), ValExpr (Maybe (Maybe a, Maybe b),Maybe c) (Maybe c))
split3CV = let combedcv = comb3CV
  in
  (fst3 Declaration.Declaration.<$> combedcv, snd3 Declaration.Declaration.<$> combedcv, thd3 Declaration.Declaration.<$> combedcv)
