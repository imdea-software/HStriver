{-# Language GADTs #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language BangPatterns #-}
module Declaration.Spec where
import Engine.Table
import Engine.Pointer
import Engine.TickExprs
import Engine.ValExprs
import Engine.TauExprs
import Declaration.Declaration
import Declaration.DecDyn
import Data.Aeson(Value)
import Data.Aeson.Types()
import Control.Monad
import Control.Lens
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import Debug.Trace

-- Datatypes

type ShowEvent = (Ident, MaybeOutside (TimeT, Value))
type DynEvent = (Ident, MaybeOutside (TimeT, Dynamic))

-- Process and show

tableFromSpec :: Specification -> Stateful [ShowEvent]
tableFromSpec spec = do
  theEvs <- dynTableFromSpec spec
  return $ map ownStr theEvs
  where
    strMap = (Map.fromList.map (first dgetId)) spec :: Map.Map Ident (Dynamic -> Value)
    ownStr (theid, x) = (theid,fmap (\(t,dyn) -> (t, strMap Map.! theid $ dyn)) x)

dynTableFromSpec :: Specification -> Stateful [DynEvent]
dynTableFromSpec spec = do
  pindexes <- mapM (getPointer.dgetId.fst) spec
  justMerge (zip3 (map (const Nothing) spec) (map (dgetId.fst) spec) pindexes)

loadEvents :: Specification -> Map.Map Ident [Event] -> Stateful ()
loadEvents spec evmaps = do
  static.eventsFun .= evmaps
  mapM_ (statefulDec.fst) spec
  static.eventsFun .= Map.empty
  !_ <- use $ static.eventsFun
  return ()

loadInputs :: TSGetter -> String -> Specification -> Map.Map Ident [Map.Map String Value] -> Stateful ()
loadInputs tsgetter fieldname spec valsmaps = do
  static.valsMaps .= valsmaps
  static.valsTSGetter .= tsgetter
  static.valsField .= fieldname
  mapM_ (statefulDec.fst) spec
  static.valsMaps .= Map.empty
  !_ <- use $ static.valsMaps
  return ()

justMerge :: [(Maybe Event, Ident, PointerIndex)] -> Stateful [DynEvent]
justMerge ls = do
  hds <- mapM dgetHead ls
  let
    minT = minimum $ map (getTS.fst3) hds
    (consumables, leave) = partition ((==minT).getTS.fst3) hds
    dynevs = catMaybes $ map dynEv consumables :: [DynEvent]
    keep = map (appFst3 Just) leave ++ map (appFst3 (const Nothing)) consumables in do
      rest <- if minT == PosInfty then return [] else justMerge keep
      return $ dynevs ++ rest

appFst3 f (a,b,c) = (f a, b, c)
fst4 (a,_,_,_) = a
appFst4 f (a,b,c,d) = (f a, b, c, d)

getHead :: (Maybe Event, (Ident, Dynamic->String, Dynamic->Value), PointerIndex) -> Stateful (Event, (Ident, Dynamic->String, Dynamic->Value), PointerIndex)
getHead (Just x, i, p) = return (x,i,p)
getHead (Nothing, i, p) = do
  ev <- liftM (fromMaybe $ error "pulled reentrant") $ pull p
  return (ev, i, p)

dgetHead :: (Maybe Event, Ident, PointerIndex) -> Stateful (Event, Ident, PointerIndex)
dgetHead (Just x, i, p) = return (x,i,p)
dgetHead (Nothing, i, p) = do
  ev <- liftM (fromMaybe $ error "pulled reentrant") $ pull p
  return (ev, i, p)

-- type DynEvent = (Ident, MaybeOutside (TimeT, Dynamic))
dynEv :: (Event, Ident, PointerIndex) -> Maybe DynEvent
dynEv (Ev (_,Nothing), _, _) = Nothing
dynEv (Ev (t,Just x), streamid, _) = Just (streamid, Ev (t, x))
dynEv (PosOutside, streamid, _) = Just (streamid, PosOutside)

-- Declaration to table:

whenM mb x = do {b<-mb; when b x}

dummy :: ILeader
dummy = return undefined

statefulDec :: DeclarationDyn -> Stateful ()
statefulDec inp@(DInp id f) = do
  evsmap <- use $ static.eventsFun
  valsmap <- use $ static.valsMaps
  tsgetter <- use $ static.valsTSGetter
  fieldval <- use $ static.valsField
  let evs = getEvsList evsmap valsmap inp tsgetter fieldval
  whenM (noLeader id) $ addInput id $ evs
statefulDec (DOut id te ve) = whenM (noLeader id) $ do
    setLeader id dummy
    !tex <- statefulTExpr te
    !vex <- statefulVExpr ve
    setLeader id (outputLeader tex vex)

getEvsList evsmap valsmap inp@(DInp sid f) tsgetter fieldval
  | null evsmap && null valsmap = error "Input with empty events"
  | null valsmap = evsmap Map.! sid
  | otherwise = let
      vallist = valsmap Map.! sid
      in map (\m -> Ev (tsgetter m, fmap f (Map.lookup fieldval m))) vallist


statefulTExpr :: TickExprDyn -> Stateful ITickExpr
statefulTExpr (DConstTE t) = constTickExpr t
statefulTExpr (DUnion te1 te2 dyn) = do
  ite1 <- statefulTExpr te1
  ite2 <- statefulTExpr te2
  unionTickExpr dyn (ite1, ite2)
statefulTExpr (DShiftTE t dec) = statefulDec dec >> shiftTickExpr t (dgetId dec)
statefulTExpr (DDelayTE dd dec) = statefulDec dec >> delayTickExpr dd (dgetId dec)

statefulVExpr :: ValExprDyn -> Stateful IValExpr
statefulVExpr (DLeaf x) = return $ constValExpr x
statefulVExpr DCV = return $ cvValExpr
statefulVExpr (DOrNoTick x y) = do
  ve0 <- statefulVExpr x
  ve1 <- statefulVExpr y
  return $ orValExpr ve0 ve1
statefulVExpr (DTau texp) = do
  itexp <- statefulTauExpr statefulDec texp
  return $ tau2ValExpr itexp
statefulVExpr (DProj DTauT _) = error "Projecting T not allowed"
statefulVExpr (DProj dtexp f) = do
  itexp <- statefulTauExpr statefulDec dtexp
  return $ proj2ValExpr f itexp
statefulVExpr (DApp x y) = do
  ve0 <- statefulVExpr x
  ve1 <- statefulVExpr y
  return $ appValExpr ve0 ve1
