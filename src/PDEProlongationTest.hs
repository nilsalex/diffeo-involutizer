module PDEProlongationTest where

import MultiIndex
import PDE
import PDEProlongation
import qualified Data.Sequence as S
import qualified Data.Map as M

testDataPDE1 :: PDESystem Int
testDataPDE1 = PDESys 5 $ S.fromList testDataPDE1List

testDataPDE1List :: [PDE Int]
testDataPDE1List =
  [
    pdeFromMap 5 $ M.fromList [(fromList 5 [1], Constant (-3)), (empty 5, coeffFromConstIdep 3 4)],
    pdeFromMap 5 $ M.fromList [(single 5 0, Constant 2), (empty 5, Constant (-3))]
  ]

testDataPDE1Prolonged :: PDESystem Int
testDataPDE1Prolonged = systemProlongations testDataPDE1
