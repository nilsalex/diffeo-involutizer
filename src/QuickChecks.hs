import MultiIndex
import PDE
import Test.QuickCheck

main :: IO Bool
main = do
        runMIxTests
        runPDETests
