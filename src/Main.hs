module Main where
-- import Examples.RobustSTLSpec
-- import Examples.Examples
-- import Examples.WindowEx
import qualified Examples.TVEx as TVEx(cost_no_delays,spec_no_delays,spec_with_delays)
import InFromFile
import System.Environment
import System.IO
import Declaration.StaticAnalysis
import HStriver(Specification)
import qualified Examples.Throughput as Throughput (main)
import qualified Examples.Cost as Cost
import Theories.Ambient
import qualified Examples.Exp1 as Exp1
import qualified Examples.Exp2 as Exp2
import qualified Examples.Exp3and4 as Exp3and4

data RunMode = ExecSpec Specification String String | AnalyseImportedSpec | Throughput Int | ShowHelp | Experiment Int

importedSpec :: Specification
importedSpec = TVEx.spec_no_delays
-- importedSpec = TVEx.cost_no_delays

main :: IO ()
main = parseArgs <$> getArgs >>= runInMode

parseArgs :: [String] -> RunMode
parseArgs ["Throughput",n] = Throughput (read n)
parseArgs ["Cost", dir,p,aps,sps,pe,wu,gts] = ExecSpec (TVEx.cost_no_delays (Cost.CT (fromIntegral$read p) (read aps) (read sps) (read pe) (read wu) (read gts))) dir "Value"
parseArgs ["--analyse"] = AnalyseImportedSpec
parseArgs ["--execute", dir, valfield] = ExecSpec importedSpec dir valfield
parseArgs ["experiment",i] = Experiment (read i)
parseArgs _ = ShowHelp

runInMode :: RunMode -> IO ()
runInMode (ExecSpec spec dir valfield) = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecJSON sysTimeGetter (Files dir valfield) spec
runInMode AnalyseImportedSpec = analyse importedSpec
runInMode (Experiment n) = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runExperiment n
runInMode (Throughput n) = Throughput.main n
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  HStriver --analyse"
  , "  HStriver --execute dir valueField"
  , "  HStriver Throughput"
  , "Where dir indicates the directory to look for the input JSONs, and valueField is the field where the value of the events is placed."
  , "Modify Main.hs to specify the imported the spec to --analyse or --execute."]

runExperiment :: Int -> IO ()
runExperiment n
 | n == 1 = runSpecJSON sysTimeGetterIgnoreSeconds sourcesmode Exp1.spec
 | n == 2 = runSpecJSON sysTimeGetter sourcesmode Exp2.spec
 | n == 3 = runSpecJSON sysTimeGetter sourcesmode Exp3and4.spec3
 | n == 4 = runSpecJSON sysTimeGetter sourcesmode Exp3and4.spec4
 where sourcesmode = Files "ambient" "Value"
