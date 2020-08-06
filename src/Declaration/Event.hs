{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Declaration.Event (module Declaration.Event, Dyn.Dynamic, Dyn.toDyn, Dyn.fromDyn, Dyn.fromDynamic, Dyn.dynApp, Dyn.Typeable) where
import Data.Maybe
import Data.Aeson
import GHC.Generics
import qualified Data.Dynamic as Dyn
import Data.Time

-- type TimeT = UTCTime
-- type TimeTDiff = NominalDiffTime
type TimeT = Double
type TimeTDiff = Double
data Time = T TimeT | NegInfty | PosInfty deriving (Eq,Show, Generic, ToJSON)
type Val = Maybe Dyn.Dynamic

maxT :: Time
maxT = PosInfty
-- maxT = T $ UTCTime (fromGregorian 2017 2 25) 0

instance Num UTCTime where
  negate      = error "TimeT is not int"
  (+)         = error "TimeT is not int"
  (*)         = error "TimeT is not int"
  fromInteger = error "TimeT is not int"
  abs         = error "TimeT is not int"
  signum      = error "TimeT is not int"

type Event = MaybeOutside (TimeT, Val)

data MaybeOutside x = NegOutside | PosOutside | Ev x deriving Show
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

posOutside :: Event
posOutside = PosOutside
negOutside :: Event
negOutside = NegOutside

isPosOutside :: Event -> Bool
isPosOutside PosOutside = True
isPosOutside _ = False

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

tDiffAdd :: TimeT -> TimeTDiff -> TimeT
-- tDiffAdd = flip addUTCTime
tDiffAdd = (+)

timeDiffPlus :: Time -> TimeTDiff -> Time
timeDiffPlus (T x) y = T (x `tDiffAdd` y)
timeDiffPlus x _ = x

unT :: Time -> TimeT
unT (T x) = x
