import Test.HUnit
import qualified TriangleMapTest
import qualified PDETest

main = do
        runTestTT TriangleMapTest.testTriangleMap
        runTestTT PDETest.testPDE
        return ()
