import PDE
import PDEProlongation
import DiffeoEquationsArea

import System.Random

getAreaPDE :: PDESystem Rational
getAreaPDE = areaPDE

main :: IO ()
main = do
        gen <- getStdGen
        let pdeSys = getAreaPDE
        putStr $ prettyPDEMatrix $ evalPDESystemRand gen $ concatPDESystems pdeSys $ systemProlongations pdeSys
