module Pure.Tests
  ( pureTests,
  )
where

import Pure.Main.Tests (mainTests)
import Test.Tasty (TestTree, testGroup)

pureTests :: TestTree
pureTests = testGroup "Pure tests" [mainTests]
