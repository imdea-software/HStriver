module InFromFile where
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy.Char8 as BS8(unpack)
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.State
import Declaration.Declaration
import Declaration.DecDyn
import Engine.Table
import Declaration.Spec
import System.FilePath.Posix(dropExtension, takeBaseName, takeExtension, (</>), FilePath)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Extra (partitionM)
import Debug.Trace

type FromJSONers = Map.Map Ident (Value -> Dynamic)

data SourcesMode = STDIN | Files {path :: String, fieldName :: String}

simpletsgetter :: String -> TSGetter
simpletsgetter fieldname m = (fromMaybe (error "aca").parseMaybe parseJSON) (m !!! fieldname)

runSpecJSON :: TSGetter -> SourcesMode -> Specification -> IO ()
runSpecJSON getts src@(Files _ fieldval) decs = do
  nameandcontents <- getNameAndContents src
  let
    nameAndValues = map (\(x,y) -> (x,map (fromJust.decode) $ takeWhile (not.B.null) $ B.split (BS.c2w '\n') y)) nameandcontents
    contentMap = Map.fromList nameAndValues
    instants = evalState (loadInputs getts fieldval decs contentMap >> tableFromSpec decs) initTable :: [ShowEvent]
    showInstants = map (\(id, mval) -> BS8.unpack$encode $ Map.fromList (("id", toJSON id):pairsfrommval mval)) instants :: [String]
    pairsfrommval PosOutside = [("PosOutside", toJSON True)]
    pairsfrommval NegOutside = [("NegOutside", toJSON True)]
    pairsfrommval (Ev (x, val)) = [("timestamp", toJSON x), ("value", val)]
--type ShowEvent = (Ident, MaybeOutside (TimeT, Value))
    in
    mapM_ putStrLn showInstants
    --putStrLn (run JSON debug decs instants)

getFiles :: String -> IO [String]
getFiles dir = do
  names <- listDirectory dir
  let paths = map (dir </>) names
  (dirs, files) <- partitionM doesDirectoryExist paths
  let herefiles = filter ((==".json").takeExtension) files
  innerfiles <- concat Prelude.<$> mapM getFiles dirs
  return $ herefiles ++ innerfiles

getNameAndContents (Files dir _) = do
  fnames <- getFiles dir
  incontents <- mapM B.readFile fnames
  return $ zip (map (dropExtension.dropWhile (=='/') . drop (length dir)) fnames) incontents
getNameAndContents STDIN = do
  content <- B.getContents
  return [("/stdin", content)]

instantsGetter contentsmap getts src inp@(DInp _ _ f) = let
  --getts m = (fromJust.(parseMaybe parseJSON) :: Value -> TimeT) (m Map.! "timestamp")
  id = dgetId inp
  evmaps = contentsmap !!! contentix src id
  in catMaybes (map (\m -> (daf (getts m) $ Map.lookup (valix src id) m)) evmaps)
  where
    daf dat mval = Just $ Ev (dat, fmap f mval)

contentix (Files _ _) = id
contentix STDIN = const "/stdin"

valix (Files _ s) _ = s
valix STDIN strmid = strmid
-- daf MultiSource dat (Just val) = Just $ Ev (dat, Just $ f val)
-- daf MultiSource dat Nothing = Nothing


-- runSpecCSV :: Bool -> Specification -> IO ()
-- runSpecCSV debug decs =
--   do
--   content <- getContents
--   let csvs = map (++"\n") $ lines content
--       (hd:instants) = concatMap (fromRight (error "No Right") . (parse csvFile "(stdin)")) csvs
--       decodedinstants = map (getInstants (getReaders decs) hd) instants
--     in putStrLn $ run CSV debug decs decodedinstants
