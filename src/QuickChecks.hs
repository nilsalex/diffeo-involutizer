module QuickChecks (qcmain) where

import MultiIndex
import PDE
import Test.QuickCheck

qcmain :: IO Bool
qcmain = do
          runMIxTests
          runPDETests
