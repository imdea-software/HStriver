{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Declaration.Event (module Declaration.Event, Dyn.Dynamic, Dyn.toDyn, Dyn.fromDyn, Dyn.fromDynamic, Dyn.dynApp, Dyn.Typeable, module TimeDomain.TimeT) where
import Data.Maybe
import Data.Aeson
import GHC.Generics
import qualified Data.Dynamic as Dyn
import TimeDomain.TimeT

type Val = Maybe Dyn.Dynamic

type Event = MaybeOutside (TimeT, Val)

data MaybeOutside x = NegOutside | PosOutside | Ev x deriving (Show, Eq)
instance Functor MaybeOutside where
  fmap f (Ev x) = Ev (f x)
  fmap _ NegOutside = NegOutside
  fmap _ PosOutside = PosOutside

type Ident = String

data DelayDir = Positive | Negative deriving Eq

getTS :: Event -> Time
getTS NegOutside = NegInfty
getTS PosOutside = PosInfty
getTS (Ev (x, _)) = T x

getVal :: MaybeOutside (a,b) -> b
getVal (Ev (_, y)) = y

isnotick :: Event -> Bool
isnotick (Ev (_,v)) = isNothing v
isnotick _ = True

isinside :: Event -> Bool
isinside (Ev _) = True
isinside _ = False

getEvent :: MaybeOutside (a,b) -> (a,b)
getEvent (Ev x) = x

posOutside :: Event
posOutside = PosOutside
negOutside :: Event
negOutside = NegOutside

isPosOutside :: Event -> Bool
isPosOutside PosOutside = True
isPosOutside _ = False

isNegOutside :: Event -> Bool
isNegOutside NegOutside = True
isNegOutside _ = False

instance Ord Time where
  compare (T x) (T y) = compare x y
  compare NegInfty NegInfty = EQ
  compare NegInfty _ = LT
  compare _ NegInfty = GT
  compare PosInfty PosInfty = EQ
  compare PosInfty _ = GT
  compare _ PosInfty = LT

makeEv :: Time -> Val -> Event
makeEv NegInfty _ = NegOutside
makeEv PosInfty _ = PosOutside
makeEv (T t) v = Ev (t, v)

-- timePlus :: Time -> TimeT -> Time
-- timePlus (T x) y = T (x+y)
-- timePlus x _ = x

unT :: Time -> TimeT
unT (T x) = x

timeDiffPlus :: Time -> TimeDiff -> Time
timeDiffPlus (T x) y = T (x `tDiffAdd` y)
timeDiffPlus x _ = x
