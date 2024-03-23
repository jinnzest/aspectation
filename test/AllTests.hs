import Pure.Tests (pureTests)
import System.IO (IO)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests" [pureTests]
