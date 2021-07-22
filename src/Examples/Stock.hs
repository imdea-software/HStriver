{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.Stock where
import Syntax.Booleans
import Syntax.Num()
import Data.Maybe
import Lib.Utils
import qualified Prelude as P ((<))
import HStriver
import Data.Maybe

spec :: Specification
spec = [out stock]

sale :: Stream Int
sale = Input "sale"

arrival :: Stream Int
arrival = Input "arrival"

stock :: Stream Int
stock = let
  ticks = ticksTE sale :+ ticksTE arrival
  vals = let
    (msal,marr) = splitCV
    sal = (fromMaybe 0) <$> msal
    arr = (fromMaybe 0) <$> marr
    in
    stock @< (t?|0) + arr - sal
  in
  "stock" =: (ticks, vals)
