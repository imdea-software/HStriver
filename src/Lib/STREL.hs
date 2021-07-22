module Lib.STREL where
import Syntax.HSPrelude hiding (and, or)
import Declaration.Declaration
import Syntax.Booleans
import Syntax.Num()
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prelude as P ((<), (>), (<=), (>=), (&&))
import Theories.Dijkstra

-- type Node = String
-- type Weight = Double
-- type Graph = Map.Map Node (Set.Set (Node, Weight))
-- SpatioTemporal Formula
type Formula a = Graph -> Node -> Stream a

data NodeType = BusStop | Hospital | MetroStop | MainSquare | Museum deriving Eq

neighbours :: Graph -> Node -> Set.Set (Node, Weight)
neighbours = (Map.!)

-- I don't think it works for paths with negative weights

reaches :: (Weight, Weight) -> Formula Bool -> Formula Bool -> Formula Bool
reaches (n,m) phi psi gr no = "reaches" <: (n,m) <: phi gr no <: psi gr no <: no =: (ConstTE 0, reaches' n m no)
  where
  reaches' n' m' no'
    | m P.< 0 = Leaf False
    | n P.> 0 = checkneighs n' m' no'
    | otherwise = (psi gr no') @<~ (t ?| False) || checkneighs n' m' no'
  checkneighs n' m' no' = (phi gr no') @<~ (t ?| False) && foldl (\acc (no'', w) -> acc || (reaches (n'-w, m'-w) phi psi gr no'') @<~ (t ?| False)) (Leaf False) (neighbours gr no')

escapes :: (Weight, Weight) -> Formula Bool -> Formula Bool
escapes (n,m) phi gr no = "escapes" <: (n,m) <: phi gr no <: no =: (ConstTE 0, escapes' Set.empty no)
  where
    distances = dijkstra no gr
    escapes' visited no'
      | mydist P.<= m P.&& mydist P.>= n = phi gr no' @<~ (t ?| False)
      | otherwise = (phi gr no') @<~ (t ?| False) && Set.foldl (\acc no'' -> acc || escapes' nextvisited no'') (Leaf False) unvisitedneighs
      where
      mydist = distances Map.! no'
      myneighs = Set.map fst $ neighbours gr no'
      unvisitedneighs = Set.difference myneighs visited
      nextvisited = Set.insert no' visited
