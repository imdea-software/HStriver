{-# LANGUAGE RebindableSyntax  #-}
module Examples.RobustSTLSpec where
import HStriver
import Lib.RobustSTL as RobustSTL

spec :: Specification
spec = [out daphis, out phi, out psi]

phi :: Stream StrVal
phi = Input "phi" -- [(fromIntegral x,Just $ toDyn (P.mod y 10)) | (x,y) <- zip [0..50000000] (randoms $ mkStdGen 1 :: [Int])]

psi :: Stream StrVal
psi = Input "psi" -- [(fromIntegral x + 0.5,Just $ toDyn (P.mod y 10)) | (x,y) <- zip [0..50000000] (randoms $ mkStdGen 2 :: [Int])]

daphis :: Declaration StrVal
daphis = until (0,5) phi psi
