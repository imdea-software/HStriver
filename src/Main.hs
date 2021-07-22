module Main where
-- import Examples.RobustSTLSpec
-- import Examples.Examples
-- import Examples.WindowEx
import qualified Examples.Clock as Clock (spec)
import qualified Examples.Stock as Stock (spec)
import InFromFile
import System.Environment
import System.IO
import Declaration.StaticAnalysis
import HStriver (Specification, sysTimeGetter)
import qualified Examples.Throughput as Throughput (main)

data RunMode = ExecImportedSpec String String String | AnalyseImportedSpec | ShowHelp

importedSpec :: Specification
importedSpec = Stock.spec

main :: IO ()
main = parseArgs <$> getArgs >>= runInMode

parseArgs :: [String] -> RunMode
parseArgs ["--analyse"] = AnalyseImportedSpec
parseArgs ["--execute", dir, tsfield, valfield] = ExecImportedSpec dir tsfield valfield
parseArgs _ = ShowHelp

runInMode :: RunMode -> IO ()
runInMode (ExecImportedSpec dir tsfield valfield) = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecJSON (sysTimeGetter tsfield) (Files dir valfield) importedSpec
runInMode AnalyseImportedSpec = analyse importedSpec
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  HStriver --analyse"
  , "  HStriver --execute dir tsField valueField"
  , "Where dir indicates the directory to look for the input JSONs, and tsField valueField are the fields where the timestamp and the value of the events are placed."
  , "Modify Main.hs to specify the imported the spec to --analyse or --execute."]
