{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module TimeDomain.TimeT where
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time

type TimeT = Double
type TimeDiff = Double

type TSGetter = Map.Map String Value -> TimeT

data Time = T TimeT | NegInfty | PosInfty deriving (Eq,Show, Generic, ToJSON)
maxT :: Time
maxT = PosInfty

tDiffAdd :: TimeT -> TimeDiff -> TimeT
tDiffAdd = (+)
tDiff :: TimeT -> TimeT -> TimeDiff
tDiff = (-)

sysTimeGetter :: TSGetter
sysTimeGetter m = (fromJust.parseMaybe parseJSON) (m Map.! "Time")
