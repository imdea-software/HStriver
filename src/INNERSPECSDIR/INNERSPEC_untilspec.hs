{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_untilspec where
import GHC.Generics
import Data.Aeson
import Declaration.DecDyn (InnerSpecification(IS), bind)
import InFromFile
import System.IO
import System.Environment
import HStriver
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P


-- Custom Haskell


untilspec ::   [(TimeT, Int)] -> [(TimeT, Int)] -> InnerSpecification Int
untilspec  xs__arg ys__arg = IS [bind xs xs__arg, bind ys ys__arg] theMaxMin empty
  where
  maxStrVal = maxBound
  minStrVal = minBound

  xs :: Stream Int
  xs = input "xs"
  ys :: Stream Int
  ys = input "ys"

  empty :: Stream Bool
  empty = (let
    ticks = ConstTE 0
    val = notick
    in "empty" =: (ticks,val)
    )

  xsmins :: Stream Int
  xsmins = (let
    ticks = ticksTE xs
    val = min <$> xsmins @<(t?|maxStrVal) <*> split0CV
    in "xsmins" =: (ticks,val)
    )

  theMins :: Stream Int
  theMins = (let
    ticks = ticksTE xsmins :+ ticksTE ys
    val = min <$> ys @<~(t?|maxStrVal) <*> xsmins @<~(t?|maxStrVal)
    in "theMins" =: (ticks,val)
    )

  theMaxMin :: Stream Int
  theMaxMin = (let
    ticks = ticksTE theMins
    val = max <$> theMaxMin @<(t?|minStrVal) <*> split0CV
    in "theMaxMin" =: (ticks,val)
    )
