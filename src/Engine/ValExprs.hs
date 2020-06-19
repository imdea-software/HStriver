{-# Language BangPatterns #-}
module Engine.ValExprs (constValExpr, appValExpr, cvValExpr, tau2ValExpr, proj2ValExpr, orValExpr) where
import Engine.Table
import Data.Maybe
import Engine.TauExprs
import Control.Monad
import qualified Prelude as P((<$>))
import Prelude

constValExpr :: Dynamic -> IValExpr
constValExpr v =
  IValExpr (const $ return (Just v)) (return ())

appValExpr :: IValExpr -> IValExpr -> IValExpr
appValExpr ve1 ve2 = IValExpr (appValE ve1 ve2) (unhookPointersVE ve1 >> unhookPointersVE ve2)

cvValExpr :: IValExpr
cvValExpr =
  IValExpr (\ev -> return $ getVal ev) (return ())

orValExpr :: IValExpr -> IValExpr -> IValExpr
orValExpr ve1 ve2 = IValExpr (orValE ve1 ve2) (unhookPointersVE ve1 >> unhookPointersVE ve2)

appValE :: IValExpr -> IValExpr -> Event -> Stateful Val
appValE ve1 ve2 ev = do
  v1 <- fromJust P.<$> calculateValueAt ve1 ev
  v2 <- fromJust P.<$> calculateValueAt ve2 ev
  let !res = dynApp v1 v2
  return $ Just res

orValE :: IValExpr -> IValExpr -> Event -> Stateful Val
orValE ve1 ve2 ev = do
  v1 <- liftM (maybe (error "") id) $ calculateValueAt ve1 ev
  v2 <- liftM (maybe (error "") id) $ calculateValueAt ve2 ev
  return $ if ((fromMaybe $ error "Wrong dyn").fromDynamic) v1 then Just v2 else Nothing

tau2ValExpr :: ITauExpr -> IValExpr
tau2ValExpr itexp = genTau2Val (toDyn.getTS) itexp

proj2ValExpr :: (Event -> Dynamic) -> ITauExpr -> IValExpr
proj2ValExpr f itexp = genTau2Val f itexp

genTau2Val :: (Event -> Dynamic) -> ITauExpr -> IValExpr
genTau2Val f itexp =
  IValExpr (fmap (Just . f) . getTTau itexp) (unhookPointersTau itexp)
