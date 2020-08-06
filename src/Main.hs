module Main where
import qualified Examples.PowerTrain as PowerTrain(runspec)
import InFromFile
import System.Environment
import System.IO
import HStriver(Specification)

data RunMode = ExecSpec Specification String String | ShowHelp

main :: IO ()
main = parseArgs <$> getArgs >>= runInMode

parseArgs :: [String] -> RunMode
parseArgs ["--run", n] = ExecSpec (PowerTrain.runspec (read n)) ("run"++n) "Value"
parseArgs _ = ShowHelp

runInMode :: RunMode -> IO ()
runInMode (ExecSpec spec dir valfield) = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecJSON (simpletsgetter "timestamp") (Files dir valfield) spec
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  HStriver --run N"
  , "Where 0<N<9 indicates the experiment number."]
