{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language BangPatterns #-}
module Engine.TickExprs (unionTickExpr, constTickExpr, shiftTickExpr, delayTickExpr) where
import Declaration.Event
import Engine.Table
import Engine.Pointer
import Data.Maybe
import Control.Monad

unionTickExpr :: Dynamic -> (ITickExpr, ITickExpr) -> Stateful ITickExpr
unionTickExpr fs tes@(te1,te2) = do
  !testateix <- getTEStateIndex
  setTEState testateix (toDyn (negOutside, negOutside, NegInfty))
  return $ ITickExpr (unionTE fs tes testateix) (unhookPointersTE te1 >> unhookPointersTE te2)

constTickExpr :: TimeT -> Stateful ITickExpr
constTickExpr t = do
  !testateix <- getTEStateIndex
  setTEState testateix (toDyn False)
  return $ ITickExpr (constTE t testateix) (return ())

shiftTickExpr :: TimeTDiff -> Ident -> Stateful ITickExpr
shiftTickExpr t streamid = do
  !pointer <- getPointer streamid
  return $ ITickExpr (shiftTE t pointer) (unhookPointer pointer)

delayTickExpr :: DelayDir -> Ident -> Stateful ITickExpr
delayTickExpr dd streamid = do
  !testateix <- getTEStateIndex
  setTEState testateix (toDyn $ if dd==Negative then NegInfty else PosInfty)
  !pointer <- getPointer streamid
  return $ ITickExpr (delayTE dd pointer testateix) (unhookPointer pointer)

constTE :: TimeT -> TeStateIndex -> Stateful Event
constTE t ix = do
  dynb <- getTEState ix
  if fromDyn dynb (error "no dyn") then
    return posOutside
  else do
    setTEState ix (toDyn True)
    return $ Ev (t, Just (toDyn ()))

shiftTE :: TimeTDiff -> PointerIndex -> Stateful Event
shiftTE t pi = do
  mev <- pull pi
  return $ maybe posOutside addShift mev
  where
    addShift (Ev (a, x)) = Ev ((a `tDiffAdd` t), x)
    addShift x = x

unionTE :: Dynamic -> (ITickExpr, ITickExpr) -> TeStateIndex -> Stateful Event
unionTE fs (leftTE, rightTE) ix = do
  (leftT, rightT, lastts) <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  leftNew <- getNextUnion lastts leftT leftTE
  rightNew <- getNextUnion lastts rightT rightTE
  retTS <- return $ min (getTS leftNew) (getTS rightNew)
  setTEState ix (toDyn (leftNew, rightNew, retTS))
  cv <- return $ checkSth fs (getCVUnion leftNew (getTS rightNew), getCVUnion rightNew (getTS leftNew))
  return $ makeEv retTS cv

getNextUnion :: Time -> Event -> ITickExpr -> Stateful Event
getNextUnion lastreturn lastev te =
  let lastTSev = getTS lastev in
  if lastTSev == PosInfty || lastTSev <= lastreturn then do
    nt <- calculateNextTime te -- Inspired by the clock stream
    if getTS nt == lastreturn then calculateNextTime te else return nt
  else
    return lastev

getCVUnion :: Event -> Time -> Val
getCVUnion ev otherts =
  if getTS ev <= otherts then getVal ev else Nothing

checkSth :: Dynamic -> (Val, Val) -> Val
checkSth fs (Nothing, Nothing) = Nothing
checkSth fs (a, b) = Just $ dynApp fs (toDyn (a,b))

-- Delay
delayTE :: DelayDir -> PointerIndex -> TeStateIndex -> Stateful Event
delayTE Negative pi ix = delayNeg pi ix
delayTE Positive pi ix = delayPos pi ix

delayPos :: PointerIndex -> TeStateIndex -> Stateful Event
delayPos pi ix = do
  sum <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  ev <- liftM (fromMaybe PosOutside) $ pull pi
  setTEState ix $! (toDyn $! getNewSum sum ev)
  return $ let evts = getTS ev in
    if sum <= evts then makeEv sum (Just $ toDyn ()) else makeEv evts Nothing

getNewSum :: Time -> Event -> Time
getNewSum _ (Ev (x, Just y))
  | realy >0 && newsum < maxT = newsum
  | otherwise = PosInfty
  where
    realy = fromDyn y (error "No double")
    newsum = T (x `tDiffAdd` realy)
getNewSum t ev
  | t <= getTS ev = PosInfty
  | otherwise = t


delayNeg :: PointerIndex -> TeStateIndex -> Stateful Event
delayNeg pi ix = do
  ev <- liftM fromJust $ iterateWhile (\(Just ev) -> isnotick ev && isinside ev) (pull pi) -- Positive cycle not supported
  limit <- getTEState ix >>= return.fromMaybe (error "State with wrong dyn").fromDynamic
  setTEState ix (toDyn $ getTS ev)
  let
    evts = getTS ev
    sum = timeDiffPlus evts $ (fromDyn.fromJust) (getVal ev) (error "no time type") -- Abuse of laziness
    in
    return $ if sum >= limit then makeEv sum (Just $ toDyn ()) else makeEv evts Nothing

iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile f x = do
  res <- x
  if not $ f res then return res else iterateWhile f x
