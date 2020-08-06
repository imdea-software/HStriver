{-# LANGUAGE RebindableSyntax  #-}
{-# Language GADTs #-}
module Examples.PowerTrain where
import Prelude
import HStriver
import Lib.Utils
import qualified Lib.STL as STL

runspec :: Int -> Specification
runspec n = [out $ phi n]

phi :: Int -> Stream Bool
phi n = "phi" ==: changePointsOf (opts!!n)

opts :: [Stream Bool]
opts = [undefined,opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8]

signal :: Stream Double
signal = Input "signal"

mode :: Stream Double
mode = Input "mode"

pedal :: Stream Double
pedal = Input "pedal"

simTime = 50
eta = 1
taus = 10 + eta
zeta_min = 5;

-- preds(i).str = 'low'; % for the pedal input signal (<0.5)
-- preds(i).str = 'high'; % for the pedal input signal (>0.5)
-- preds(i).str = 'utr'; % u<=Ut
-- preds(i).str = 'utl'; % u>=-Ut
-- preds(i).str = 'pwr'; % mode >0.5 (power mode = 1)
-- preds(i).str = 'norm'; % mode < 0.5 (normal mode = 0)

low :: Stream Bool
low = "low" ==: strMap "<0.5" (<0.5) pedal
high :: Stream Bool
high = "high" ==: strMap ">0.5" (>0.5) pedal
utl :: Double -> Stream Bool
utl x = "utl" <: x ==: strMap (">-" ++ show x) (>(-x)) signal
utr ::Double ->  Stream Bool
utr x = "utr" <: x ==: strMap ("<" ++ show x) (<x) signal
pwr :: Stream Bool
pwr = "pwr" ==: strMap ">0.5" (>0.5) mode
norm :: Stream Bool
norm = "norm" ==: strMap "<0.5" (<0.5) mode

-- Opt1:
-- Ut = 0.05;
-- phi = ['[]_(' num2str(taus) ', ' num2str(simTime) ') utl /\ utr'];

ut1 = 0.05
opt1 :: Stream Bool
opt1 = STL.always (taus, simTime) (utl ut1 `STL.and` utr ut1)

-- Opt2:
-- Ut = 0.02;
-- phi = ['[]_(' num2str(taus) ', ' num2str(simTime) ')(((low/\<>_(0,' ...
--   num2str(h) ')high) \/ (high/\<>_(0,' num2str(h) ')low))' ...
--   '-> []_[' num2str(eta) ', ' num2str(zeta_min) '](utr /\ utl))'];
ut2 = 0.02
opt2 :: Stream Bool
opt2 = STL.always (taus, simTime) (((low `STL.and` STL.eventually (0,h) high) `STL.or`
                                    (high `STL.and` STL.eventually (0,h) low)) `STL.implies`
                                   STL.always (eta, zeta_min) (utl ut2 `STL.and` utr ut2))

-- Opt3 :
-- C = 0.05;
-- Ut = C;
-- phi = ['<>_['  num2str(simTime)  ','  num2str(simTime) '] utr' ];
ut3 = 0.05
opt3 :: Stream Bool
opt3 = STL.eventually (simTime, simTime) (utr ut3)

-- Opt 4:
-- Cr = 0.1;
-- Ut = Cr;
-- phi = ['[]_(' num2str(taus) ', ' num2str(simTime) ')utr'];
ut4 = 0.1
opt4 :: Stream Bool
opt4 = STL.always (taus, simTime) (utr ut4)

-- Opt 5:
-- Cr = 0.1;
-- Ut = Cl;
-- phi = ['[]_(' num2str(taus) ', ' num2str(simTime) ')utl'];
ut5 = 0.1
opt5 :: Stream Bool
opt5 = STL.always (taus, simTime) (utl ut5)

-- Opt 6:
-- Ut = 0.02;
-- phi = ['[] ((pwr /\ <>_(0,' num2str(h), ')norm) -> (<>_('num2str(eta)', ' num2str(zeta_min) ') utl /\ utr))'];

h = 0.02;
ut6 = 0.02
opt6 :: Stream Bool
opt6 = (pwr `STL.and` STL.eventually (0, h) norm) `STL.implies` STL.eventually (eta, zeta_min) (utl ut6 `STL.and` utr ut6)

-- Opt 7:
-- Ut = 0.2;
-- phi = '[] (pwr -> (utl /\ utr))';
ut7 = 0.2
opt7 :: Stream Bool
opt7 = pwr `STL.implies` (utl ut7 `STL.and` utr ut7)

-- Opt 8:
-- Ut = 0.1;
-- phi = ['[] (((low/\<>_(0,' num2str(h) ...
--     ')high) \/ (high/\<>_(0,' num2str(h) ')low)) -> [] (utr /\ utl))'];
ut8 = 0.1
opt8 :: Stream Bool
opt8 = ((low `STL.and` STL.eventually (0,h) high) `STL.or`
        (high `STL.and` STL.eventually (0,h) low)) `STL.implies` STL.always (0,simTime) (utl ut8 `STL.and` utr ut8)
