{-# Language BangPatterns #-}
module Engine.ValExprs (constValExpr, appValExpr, cvValExpr, tau2ValExpr, proj2ValExpr, iteValExpr, notickValExpr) where
import Engine.Table
import Data.Maybe
import Engine.TauExprs
import Control.Monad
import qualified Prelude as P((<$>))
import Prelude

constValExpr :: Dynamic -> IValExpr
constValExpr v =
  IValExpr (const $ return (Just v)) (return ()) (const $ return ())

notickValExpr :: IValExpr
notickValExpr = IValExpr (const $ return Nothing) (return ()) (const $ return ())

appValExpr :: IValExpr -> IValExpr -> IValExpr
appValExpr ve1 ve2 = IValExpr (appValE ve1 ve2) (unhookPointersVE ve1 >> unhookPointersVE ve2) (\t -> ffwd ve1 t >> ffwd ve2 t)

iteValExpr :: IValExpr -> IValExpr -> IValExpr -> IValExpr
iteValExpr ve1 ve2 ve3 = IValExpr (iteValE ve1 ve2 ve3) (unhookPointersVE ve1 >> unhookPointersVE ve2 >> unhookPointersVE ve3) (\t -> ffwd ve1 t >> ffwd ve2 t >> ffwd ve3 t)

cvValExpr :: IValExpr
cvValExpr =
  IValExpr (return.getVal) (return ()) (const $ return ())

appValE :: IValExpr -> IValExpr -> Event -> Stateful Val
appValE ve1 ve2 ev = do
  !v1 <- fromJust P.<$> calculateValueAt ve1 ev
  !v2 <- fromJust P.<$> calculateValueAt ve2 ev
  let !res = dynApp v1 v2
  return $ Just res

iteValE :: IValExpr -> IValExpr -> IValExpr -> Event -> Stateful Val
iteValE ve1 ve2 ve3 ev = do
  !v1 <- fromJust P.<$> calculateValueAt ve1 ev
  !mv2 <- calculateValueAt ve2 ev
  !mv3 <- calculateValueAt ve3 ev
  return $ if (fromMaybe (error "Wrong dyn").fromDynamic) v1 then mv2 else mv3

tau2ValExpr :: ITauExpr -> IValExpr
tau2ValExpr itexp = genTau2Val (toDyn.getTS) itexp

proj2ValExpr :: (Event -> Dynamic) -> ITauExpr -> IValExpr
proj2ValExpr f itexp = genTau2Val f itexp

genTau2Val :: (Event -> Dynamic) -> ITauExpr -> IValExpr
genTau2Val f itexp =
  IValExpr (fmap (Just . f) . getTTau itexp) (unhookPointersTau itexp) (ffwdTau itexp)
