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
import qualified Data.Sequence as S 

main = do
--        runMIxTests
        putStrLn $ prettyMIx $ empty 5
        putStrLn $ prettyMIx $ single 5 3
        putStrLn $ prettyMIx $ addIndex 1 $ addIndex 3 $ single 5 3
