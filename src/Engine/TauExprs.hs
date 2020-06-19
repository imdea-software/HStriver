{-# Language BangPatterns #-}
module Engine.TauExprs where
import Engine.Table
import Data.Maybe
import Declaration.Declaration
import Declaration.DecDyn
import Engine.Pointer
import Control.Monad

data ITauExpr = ITauExpr {
  getTTau :: Event -> Stateful Event,
  unhookPointersTau :: Stateful ()
                           }

statefulTauExpr :: (DeclarationDyn -> Stateful ()) -> TauExprDyn -> Stateful ITauExpr
statefulTauExpr _ DTauT = return $ ITauExpr return (return ())
statefulTauExpr f (DPrev dec texp) = f dec >> statefulTauExpr f texp >>= genPrev False dec
statefulTauExpr f (DPrevEq dec texp) = f dec >> statefulTauExpr f texp >>= genPrev True dec
statefulTauExpr f (DSucc dec texp) = f dec >> statefulTauExpr f texp >>= genSucc False dec
statefulTauExpr f (DSuccEq dec texp) = f dec >> statefulTauExpr f texp >>= genSucc True dec
statefulTauExpr f (DBoundedSucc b dec texp) = f dec >> statefulTauExpr f texp >>= boundedSucc b dec

genPrev :: Bool -> DeclarationDyn -> ITauExpr -> Stateful ITauExpr
genPrev isEq dec itexp = do
  !pindex <- getPointer $ dgetId dec
  stateix <- getTEStateIndex
  setTEState stateix $ toDyn (negOutside, negOutside)
  return $ ITauExpr (getPrev isEq stateix pindex itexp) (unhookPointer pindex >> unhookPointersTau itexp)

getPrev :: Bool -> TeStateIndex -> PointerIndex -> ITauExpr -> Event -> Stateful Event
getPrev isEq ix pindex tauexpr ev = do
  evs@(lastret, headev) <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  innerev <- getTTau tauexpr ev
  t <- return $ getTS innerev
  (newlastret, newhead) <- getNewVals pindex t evs
  newlastret <- return $ if isEq && getTS newhead == t && not (isnotick newhead) then newhead else newlastret
  setTEState ix $ toDyn (newlastret, newhead)
  return newlastret

getNewVals :: PointerIndex -> Time -> (Event, Event) -> Stateful (Event, Event)
getNewVals pindex t (lastret, headev)
  | t <= getTS headev = return (lastret, headev)
  | otherwise = do
      newlastret <- return $ if isnotick headev then lastret else headev
      mev <- pull pindex
      if isNothing mev then return (newlastret, headev) else
          getNewVals pindex t (newlastret, maybe (error "") id mev)

genSucc :: Bool -> DeclarationDyn -> ITauExpr -> Stateful ITauExpr
genSucc isEq dec itexp = do
  pindex <- getPointer $ dgetId dec
  stateix <- getTEStateIndex
  setTEState stateix $ toDyn negOutside
  return $ ITauExpr (getSucc isEq stateix pindex itexp) (unhookPointer pindex >> unhookPointersTau itexp)

getSucc :: Bool -> TeStateIndex -> PointerIndex -> ITauExpr -> Event -> Stateful Event
getSucc isEq ix pindex tauexpr ev = do
  headev <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  innerev <- getTTau tauexpr ev
  t <- return $ getTS innerev
  h <- getNewHead pindex (if isEq then (<=) else (<)) t headev
  setTEState ix $ toDyn h
  return h

getNewHead :: PointerIndex -> (Time -> Time -> Bool) -> Time -> Event -> Stateful Event
getNewHead pindex f t headev
  | isPosOutside headev = return headev
  | t `f` getTS headev = return headev
  | otherwise = do
      ev <- liftM (maybe (error "") id) $ pull pindex
      nextev <- return $ if isnotick ev && isinside ev then headev else ev
      getNewHead pindex f t nextev

boundedSucc :: TimeTDiff -> DeclarationDyn -> ITauExpr -> Stateful ITauExpr
boundedSucc bound dec itexp = do
  pindex <- getPointer $ dgetId dec
  stateix <- getTEStateIndex
  setTEState stateix $ toDyn negOutside
  return $ ITauExpr (getBoundedSucc bound stateix pindex itexp) (unhookPointer pindex >> unhookPointersTau itexp)

getBoundedSucc :: TimeTDiff -> TeStateIndex -> PointerIndex -> ITauExpr -> Event -> Stateful Event
getBoundedSucc bound ix pindex tauexpr ev = do
  headev <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  innerev <- getTTau tauexpr ev
  t <- return $ getTS innerev
  h <- getNewHeadBound bound pindex t headev
  setTEState ix $ toDyn h
  return $ if isnotick h || getTS h > t `timeDiffPlus` bound then posOutside else h

getNewHeadBound :: TimeTDiff -> PointerIndex -> Time -> Event -> Stateful Event
getNewHeadBound bound pindex t headev
  | isPosOutside headev = return headev
  | (t < getTS headev && not (isnotick headev)) || t `timeDiffPlus` bound <= getTS headev = return headev
  | otherwise = do
      ev <- fromMaybe (error "") Prelude.<$> pull pindex
      getNewHeadBound bound pindex t ev
