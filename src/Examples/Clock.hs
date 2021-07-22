{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Examples.Clock where
import Syntax.Num()
import Data.Maybe
import HStriver
import Data.Time
import Data.Aeson
import GHC.Generics

clock :: Stream TimeDiff
clock = let
  ticks = ConstTE 0 :+ DelayTE Positive clock
  val = 5
  in "clock" =: (ticks,val)

spec :: Specification
spec = [out clock]
