import IntertwinerArea
import IntertwinerMetric
import IntertwinerSym3
import MultiIndex
import PDE
import PDEProlongation
import Test.QuickCheck
import DiffeoEquationsArea

import Data.Ratio

import System.Random

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Sequence as S 

getAreaPDE :: PDESystem Double
getAreaPDE = areaPDE

main = do
--        runMIxTests
        gen <- getStdGen
        putStr $ prettyPDESystem $ evalPDESystemRand gen 315 $ systemProlongations 315 getAreaPDE
