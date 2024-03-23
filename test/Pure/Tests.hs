module Pure.Tests
  ( pureTests,
  )
where

import Pure.Main.Tests (mainTests)
import Pure.Shared.Tests (sharedTests)
import Test.Tasty (TestTree, testGroup)

pureTests :: TestTree
pureTests = testGroup "Pure tests" [sharedTests, mainTests]
