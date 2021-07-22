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
import Data.Aeson(Value, encode)
import Data.Aeson.Types(toJSON)
import Control.Monad
import Control.Lens
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import Debug.Trace
import Control.Monad.State
import GHC.Stack
import qualified Data.ByteString.Lazy.Char8 as BS8(unpack)

-- Datatypes

type ShowEvent = (Ident, MaybeOutside (TimeT, Value))
type DynEvent = (Ident, MaybeOutside (TimeT, Dynamic))

type MergeState = (Maybe Event, Ident, PointerIndex)

-- Process and show

x !!! y = fromMaybe (error "no key") (x Map.!? y)

tableFromSpec :: Specification -> Stateful [ShowEvent]
tableFromSpec spec = do
  theEvs <- filter (\(id,_) -> id /= "ignoreme") Prelude.<$> dynTableFromSpec spec
  return $ map ownStr theEvs
  where
    strMap = (Map.fromList.map (first dgetId)) spec :: Map.Map Ident (Dynamic -> Value)
    ownStr (theid, x) = (theid,fmap (\(t,dyn) -> (t, strMap !!! theid $ dyn)) x)

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

justMerge :: [MergeState] -> Stateful [DynEvent]
justMerge ls = do
  hds <- mapM dgetHead ls
  let
    minT = minimum $ map (getTS.fst3) hds
    (consumables, leave) = partition ((==minT).getTS.fst3) hds
    dynevs = catMaybes $ map dynEv consumables :: [DynEvent]
    comeon [] = [("ignoreme", NegOutside)]
    comeon x = x
    keep = map (appFst3 Just) leave ++ map (appFst3 (const Nothing)) consumables in do
      rest <- if minT == PosInfty then return [] else justMerge keep
      return $ (comeon dynevs) ++ rest

appFst3 f (a,b,c) = (f a, b, c)
fst4 (a,_,_,_) = a
appFst4 f (a,b,c,d) = (f a, b, c, d)

dgetHead :: MergeState -> Stateful (Event, Ident, PointerIndex)
dgetHead (Just x, i, p) = return (x,i,p)
dgetHead (Nothing, i, p) = do
  ev <- pull p
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
statefulDec inp@(DInp _ _ f) = do
  evsmap <- use $ static.eventsFun
  valsmap <- use $ static.valsMaps
  tsgetter <- use $ static.valsTSGetter
  fieldval <- use $ static.valsField
  let evs = getEvsList evsmap valsmap inp tsgetter fieldval
  let id = dgetId inp
  whenM (noLeader id) $ addInput id $ evs
statefulDec (DOut id te ve) = whenM (noLeader id) $ do
    setLeader id dummy
    !tex <- statefulTExpr te
    !vex <- statefulVExpr ve
    setLeader id (outputLeader tex vex)

getEvsList evsmap valsmap inp@(DInp _ _ f) tsgetter fieldval
  | null evsmap && null valsmap = error "Input with empty events"
  | null valsmap = evsmap !!! sid
  | otherwise = let
      vallist = valsmap !!! sid
      in map (\m -> Ev (tsgetter m, fmap f (Map.lookup fieldval m))) vallist
  where sid = dgetId inp


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
statefulVExpr DCV = return cvValExpr
statefulVExpr DNotick = return notickValExpr
statefulVExpr (DTau texp) = do
  !itexp <- statefulTauExpr statefulDec texp
  return $ tau2ValExpr itexp
statefulVExpr (DProj DTauT _) = error "Projecting T not allowed"
statefulVExpr (DProj dtexp f) = do
  !itexp <- statefulTauExpr statefulDec dtexp
  return $ proj2ValExpr f itexp
statefulVExpr (DApp x y) = do
  !ve0 <- statefulVExpr x
  !ve1 <- statefulVExpr y
  return $ appValExpr ve0 ve1
statefulVExpr (DITE x y z) = do
  !ve0 <- statefulVExpr x
  !ve1 <- statefulVExpr y
  !ve2 <- statefulVExpr z
  return $ iteValExpr ve0 ve1 ve2

runSpec :: forall a. Typeable a => InnerSpecification a -> Maybe a
runSpec innerspec@(IS ins retStr stopStr) = let
  spec = getDecs innerspec
  retstrid = getId retStr
  stopstrid = getId stopStr 
  damap = Map.fromList ins
  events = evalState (loadEvents spec damap >> fullTableFromIS (retstrid, stopstrid)) initTable
  in processEvents (\x -> fromDyn x (error "wrongdyn") :: a) retstrid stopstrid events
  -- loadEvents :: Specification -> Map.Map Ident [Event] -> Stateful ()

runSpecMocked :: Specification -> [(Ident, [Event])] -> [String]
runSpecMocked spec ins = let
  damap = Map.fromList ins
  events = evalState (loadEvents spec damap >> tableFromSpec spec) initTable
  showInstants = map (\(id, mval) -> BS8.unpack$encode $ Map.fromList (("id", toJSON id):pairsfrommval mval)) events :: [String]
  pairsfrommval PosOutside = [("PosOutside", toJSON True)]
  pairsfrommval NegOutside = [("NegOutside", toJSON True)]
  pairsfrommval (Ev (x, val)) = [("timestamp", toJSON x), ("value", val)]
  in showInstants

processEvents :: (Dynamic -> a) -> Ident -> Ident -> [(Ident, Event)] -> Maybe a
processEvents transformer retStr stopStr events = procevs Nothing events
  where
  procevs ma [] = ma
  procevs ma ((hdid,hdv):rest)
    | isnotick hdv = procevs ma rest
    | hdid == retStr = procevs (Just val) rest
    | hdid == stopStr && stop = ma
    | otherwise = procevs ma rest
    where
    dval = fromJust$getVal hdv
    val = transformer dval
    stop = fromDyn dval (error "wrong bool")

fullTableFromIS :: (Ident, Ident) -> Stateful [(Ident, Event)]
fullTableFromIS (retstrid, stopstrid) = do
  retpointer <- getPointer retstrid
  stoppointer <- getPointer stopstrid
  myMerge [(Nothing, retstrid, retpointer), (Nothing, stopstrid, stoppointer)]
  -- The order is important!

myMerge :: [MergeState] -> Stateful [(Ident, Event)]
myMerge ls = do
  hds <- mapM dgetHead ls
  let minT = minimum $ map (getTS.fst3) hds
  let (consumables, leave) = partition ((==minT).getTS.fst3) hds
  let keep = map (appFst3 Just) leave ++ map (appFst3 (const Nothing)) consumables
  rest <- if minT == PosInfty then return [] else myMerge keep
  return $ map (\(ev,id,_) -> (id,ev)) consumables ++ rest
