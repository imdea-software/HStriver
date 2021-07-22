module Declaration.StaticAnalysis where
import Declaration.DecDyn
import Declaration.Declaration
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe
import System.Exit

type Grapher = State (Set.Set Ident, Graph)
type Lister = State (Set.Set Ident, [DeclarationDyn]) ()

data Sign = Present | Past | Future deriving (Eq, Ord, Show)
type Arrow = (Ident, Ident, Sign)
type Graph = Set.Set Arrow
type FGraph = Map.Map Ident (Map.Map Ident (Set.Set Sign))

analyse :: Specification -> IO ()
analyse decs = let
  alloutdecs = getAllOutDecs decs
  nofunnotick = all checkFunsNotick alloutdecs
  (dot, merr) = getGraph decs
  -- dot is the dotfile to plot the dependency graph
  showdot = "Dot file:\n" ++ dot in do
  putStrLn showdot
  case merr of
      Nothing -> if nofunnotick then exitSuccess else putStrLn "ERROR: Function with notick" >> exitWith (ExitFailure 1)
      Just err -> putStrLn err >> exitWith (ExitFailure 1)

getAllOutDecs :: Specification -> [DeclarationDyn]
getAllOutDecs spec = let
  damonad = mapM_ (lister.fst) spec
  (_,decs) = execState damonad (Set.empty, [])
  in decs

lister :: DeclarationDyn -> Lister
lister (DInp _ _ _) = return ()
lister def@(DOut streamid te ve) = do
  (ids, decs) <- get
  when (not $ Set.member streamid ids)
    (put (Set.insert streamid ids, def:decs) >> telister te >> velister ve)

telister :: TickExprDyn -> Lister
telister (DConstTE _) = return ()
telister (DUnion te1 te2 _) =
  telister te1 >> telister te2
telister (DShiftTE _ dec) = lister dec
telister (DDelayTE _ dec) = lister dec

velister :: ValExprDyn -> Lister
velister (DApp ve1 ve2) = velister ve1 >> velister ve2
velister (DITE ve1 ve2 ve3) = velister ve1 >> velister ve2 >> velister ve3
velister (DTau te) = taulister te >> return ()
velister (DProj te _) = taulister te >> return ()
velister _ = return ()

taulister :: TauExprDyn -> Lister
taulister DTauT = return ()
taulister (DPrev dec te) = lister dec >> taulister te
taulister (DPrevEq dec te) = lister dec >> taulister te
taulister (DSucc dec te) = lister dec >> taulister te
taulister (DSuccEq dec te) = lister dec >> taulister te
taulister (DBoundedSucc _ dec te) = lister dec >> taulister te

checkFunsNotick :: DeclarationDyn -> Bool
checkFunsNotick (DOut _ _ ve) = check True ve
  where
  check _ (DApp ve1 ve2) = check False ve1 && check False  ve2
  check allowed (DITE ve1 ve2 ve3) = check False ve1 && check allowed ve2 && check allowed ve3
  check allowed DNotick = allowed
  check _ _ = True

getGraph :: Specification -> (String, Maybe String)
getGraph spec = let
  grapher = mapM_ (dec2arrows.fst) spec
  (_, graph) = execState grapher (Set.empty, Set.empty)
  in (dotFromGraph graph, checkCorrect $ toFGraph graph)

toFGraph :: Graph -> FGraph
toFGraph g = foldl f Map.empty g
  where
    f damap (a,b,sign) = let
      dasignsmap = fromMaybe Map.empty (damap Map.!? a)
      dasigns = fromMaybe Set.empty (dasignsmap Map.!? b)
      newsigns = Set.insert sign dasigns
      newsignsmap = Map.insert b newsigns dasignsmap
      in Map.insert a newsignsmap damap

checkCorrect :: FGraph -> Maybe String
checkCorrect graph = let
  idents = Set.fromList $ Map.keys graph
  cycles = setMapUnion (\v -> Set.fromList $ paths graph Set.empty v v) idents
  cyclesAreFine = Set.map cycleCorrect cycles
  faultycycle = sequence_ cyclesAreFine
  taggedCycles = map (\(x:r, signs) -> (x, Right ((x:r), signs))) $ Set.toList cycles
  taggedMap = Map.fromListWith checkSigns taggedCycles
  faultynode = either (\(p1,p2) -> Left $ "Node in past and future cycles: " ++ show p1 ++ " and " ++ show p2) (const $ Right ()) (sequence_ $ Map.elems taggedMap)
  faultysth = faultycycle >> faultynode
  in either (Just) (const Nothing) faultysth
  where
    cycleCorrect (path, signs)
      | Set.isSubsetOf pastfutSigns signs = Left ("Mixed past and future cycle: " ++ show path)
      | Set.member Present signs = Left ("Present cycle: " ++ show path)
      | otherwise = Right ()
    checkSigns (Right (p1,s1)) (Right (p2,s2))
      | s1 == s2 = Right (p1,s1)
      | otherwise = Left (p1, p2)
    checkSigns (Left x) _ = Left x
    checkSigns _ x = x

paths :: FGraph -> Set.Set Ident -> Ident -> Ident -> [([Ident], Set.Set Sign)]
paths graph forbidden a b = let
    neighs = fromMaybe Map.empty $ graph Map.!? a
    tail = maybe [] (\signs -> [([a,b], signs)]) $ neighs Map.!? b
    fneighs = Map.filterWithKey (\k _ -> Set.notMember k forbidden && k/=a) neighs
    neighpaths = concatMap (processneighbor (Set.insert a forbidden)) (Map.toList fneighs)
    in neighpaths ++ tail
  where
    processneighbor newfb (n, signs) = let
      ps = paths graph newfb n b
      ps' = if n==b then ([b],signs):ps else ps
      in [(a:(fst p), merge signs (snd p)) | p <- ps']
    merge x y
      | Set.isSubsetOf pastfutSigns $ Set.union x y = pastfutSigns
      | Set.member Present x = y
      | otherwise = x

dec2arrows :: DeclarationDyn -> Grapher ()
dec2arrows (DInp _ _ _) = return ()
dec2arrows (DOut streamid te ve) = do
  (set, graph) <- get
  when (not $ Set.member streamid set)
    (put (Set.insert streamid set, graph) >>
    te2arrows streamid te >>
    ve2arrows streamid ve)

te2arrows :: Ident -> TickExprDyn -> Grapher ()
te2arrows _ (DConstTE _) = return ()
te2arrows streamid (DUnion te1 te2 _) =
  te2arrows streamid te1 >> te2arrows streamid te2
te2arrows streamid (DShiftTE t dec) = getArrows streamid (getSign t) dec
  where
    getSign t
      | t == 0 = Present
      | t > 0 = Past
      | t < 0 = Future
te2arrows streamid (DDelayTE dd dec) = getArrows streamid (getSign dd) dec
  where
    getSign Positive = Past
    getSign Negative = Future

getArrows :: Ident -> Sign -> DeclarationDyn -> Grapher ()
getArrows streamid sgn dec = do
  dec2arrows dec
  (set, graph) <- get
  put (set, Set.insert (streamid, dgetId dec, sgn) graph)


ve2arrows :: Ident -> ValExprDyn -> Grapher ()
ve2arrows streamid (DApp ve1 ve2) = ve2arrows streamid ve1 >> ve2arrows streamid ve2
ve2arrows streamid (DITE ve1 ve2 ve3) = ve2arrows streamid ve1 >> ve2arrows streamid ve2 >> ve2arrows streamid ve3
ve2arrows streamid (DTau te) = tau2arrows streamid te >> return ()
ve2arrows streamid (DProj te _) = tau2arrows streamid te >> return ()
ve2arrows streamid _ = return ()

tau2arrows :: Ident -> TauExprDyn -> Grapher (Set.Set Sign)
tau2arrows streamid DTauT = return $ Set.singleton Present
tau2arrows streamid (DPrev dec te) = tau2arrows' streamid dec te $ tofun (pastSigns, pastSigns, allSigns)
tau2arrows streamid (DPrevEq dec te) = tau2arrows' streamid dec te $ tofun (pastSigns, pastpresSigns, allSigns)
tau2arrows streamid (DSucc dec te) = tau2arrows' streamid dec te $ tofun (allSigns, futSigns, futSigns)
tau2arrows streamid (DSuccEq dec te) = tau2arrows' streamid dec te $ tofun (allSigns, futpresSigns, futSigns)
tau2arrows streamid (DBoundedSucc _ dec te) = tau2arrows' streamid dec te $ tofun (allSigns, futSigns, futSigns)

allSigns = Set.fromList [Past, Present, Future]
pastSigns = Set.fromList [Past]
pastpresSigns = Set.fromList [Past, Present]
futSigns = Set.fromList [Future]
futpresSigns = Set.fromList [Future, Present]
presSigns = Set.fromList [Present]
pastfutSigns = Set.fromList [Past, Future]

tofun (x,_,_) Past = x
tofun (_,y,_) Present = y
tofun (_,_,z) Future = z

tau2arrows' :: Ident -> DeclarationDyn -> TauExprDyn -> (Sign -> Set.Set Sign) -> Grapher (Set.Set Sign)
tau2arrows' streamid dec te f = do
  dec2arrows dec
  sgns <- liftM (setMapUnion f) $ tau2arrows streamid te
  (set, graph) <- get
  put (set, Set.union graph (Set.map (\sign -> (streamid, dgetId dec, sign)) sgns))
  return sgns

setMapUnion :: (Ord a, Ord b) => (a-> Set.Set b) -> Set.Set a -> Set.Set b
setMapUnion f s = Set.foldl Set.union Set.empty (Set.map f s)

dotFromGraph :: Graph -> String
dotFromGraph g = let
  prefix = "digraph {\n"
  suffix =  "}\n"
  strings = concatMap (\(v0,v1,i) -> "    " ++ show v0 ++ " -> " ++ show v1 ++ "[label=\"" ++ show i ++ "\"];\n") (Set.toList g)
  in prefix ++ strings ++ suffix
