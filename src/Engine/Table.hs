{-# LANGUAGE TemplateHaskell #-} -- For makeLenses
{-# LANGUAGE FlexibleInstances #-} -- For Show ILeader
module Engine.Table (module Engine.Table, module Declaration.Event) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens
import Control.Monad.State
import Data.Maybe
import Declaration.Event
import qualified Data.Sequence as Seq
import qualified Control.Monad.Fail as Fail
import Data.Aeson(Value)

type TeStateIndex = Int
type Stateful = State Table

type TSGetter = Map.Map String Value -> TimeT

instance Fail.MonadFail Identity where
  fail = error "fail"

type EvList = Seq.Seq Event
type ILeader = Stateful Event
type PointerIndex = (Ident, Int)

data ITickExpr = ITickExpr {
  calculateNextTime :: Stateful Event,
  unhookPointersTE :: Stateful ()
                           }

data IValExpr = IValExpr {
  calculateValueAt :: Event -> Stateful Val,
  unhookPointersVE :: Stateful (),
  ffwd :: TimeT -> Stateful ()
                           }

instance Show ILeader where
  show _ = "ILeader"

instance Show (Dynamic -> String) where
  show _ = "shower"

data StaticTable = StaticTable {
  _eventsFun :: Map.Map Ident [Event],
  _valsMaps :: Map.Map Ident [Map.Map String Value],
  _valsTSGetter :: TSGetter,
  _valsField :: String,
  _leaders :: Map.Map Ident ILeader,
  _pindex :: Int,
  _inputs :: Map.Map Ident [Event],
  _tesindex :: TeStateIndex,
  _showers :: Map.Map Ident (Dynamic -> String)
  }

data DynTable = DynTable {
  _resolving :: Set.Set Ident,
  _pointers :: Map.Map Ident (Map.Map Int EvList),
  _testates :: Map.Map TeStateIndex Dynamic
} deriving Show

data Table = Table {
  _dyn :: DynTable,
  _static :: StaticTable
}

initTable :: Table
initTable = Table {
  _dyn = DynTable {
      _resolving = Set.empty,
      _pointers = Map.empty,
      _testates = Map.empty
                  },
  _static = StaticTable {
      _eventsFun = Map.empty,
      _valsMaps = Map.empty,
      _valsTSGetter = undefined,
      _valsField = undefined,
      _leaders = Map.empty,
      _pindex = 0,
      _inputs = Map.empty,
      _tesindex = 0,
      _showers = Map.empty
                        }
                  }

makeLenses ''StaticTable
makeLenses ''DynTable
makeLenses ''Table

getNext :: Ident -> Stateful Bool
getNext streamid = do
  resolvs <- use $ dyn.resolving
  if Set.member streamid resolvs then
    return True
  else do
    dyn.resolving .= Set.insert streamid resolvs
    mLeader <- use $ static.leaders.(at streamid)
    ev <- fromMaybe (error ("No leader found for " ++ streamid)) mLeader
    thepointers <- use $ dyn.pointers.(at streamid)
    dyn.pointers.(at streamid) .= addEvent ev thepointers
    dyn.resolving .= resolvs -- it is a stack. I think.
    return False

-- Private
addEvent :: Event -> Maybe (Map.Map Int EvList) -> Maybe (Map.Map Int EvList)
addEvent ev (Just pmap) = Just $ Map.map (addEvent' ev) pmap
  where
  addEvent' ev Empty = Seq.singleton ev
  addEvent' ev seq@(xs Seq.:|> x)
    | isnotick $ x = xs Seq.:|> ev
    | otherwise = seq Seq.:|> ev
addEvent ev Nothing = error "Adding event to not referenced stream"

setLeader :: Ident -> ILeader -> Stateful ()
setLeader streamid stev = static.leaders.(at streamid) .= Just stev

noLeader :: Ident -> Stateful Bool
noLeader streamid = use $ static.leaders.(at streamid).(to isNothing)

getPointer :: Ident -> Stateful PointerIndex
getPointer streamid = do
  pointerindex <- use $ static.pindex
  static.pindex +=1
  pointermap <- use $ dyn.pointers.(at streamid).(to $ fromMaybe Map.empty)
  dyn.pointers.(at streamid) .= Just (Map.insert pointerindex Seq.Empty pointermap)
  return (streamid, pointerindex)

addInput :: Ident -> [Event] -> Stateful ()
addInput streamid evs = do
  static.inputs.(at streamid) .= Just evs
  setLeader streamid (inputLeader streamid)

inputLeader :: Ident -> ILeader
inputLeader streamid = do
  list <- use $ static.inputs.(at streamid)._Just
  case list of
    [] -> return posOutside
    hd:rest -> do
      static.inputs.(at streamid) .= Just rest
      return hd

outputLeader :: ITickExpr -> IValExpr -> ILeader
outputLeader tickExpr valExpr = do
  ev <- calculateNextTime tickExpr
  if isPosOutside ev then
    unhookPointersTE tickExpr >> unhookPointersVE valExpr >> return posOutside
  else if isnotick ev then do
    let mayberoll (T ts) = ffwd valExpr ts
        mayberoll _ = return ()
    mayberoll (getTS ev)
    return ev
  else do
    mval <- calculateValueAt valExpr ev
    T ts <- return $ getTS ev
    return $ Ev (ts, mval)

getTEStateIndex :: Stateful TeStateIndex
getTEStateIndex = static.tesindex += 1 >> use (static.tesindex)

getTEState :: TeStateIndex -> Stateful Dynamic
getTEState ix = use $ dyn.testates.(at ix).(to (fromMaybe $ error "State not found"))

setTEState :: TeStateIndex -> Dynamic -> Stateful ()
setTEState ix dynval = dyn.testates.(at ix) .= Just dynval
