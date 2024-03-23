module Pure.Shared.Tests
  ( sharedTests,
  )
where

import Pure.Shared.Text.UtilsTests (utilsTests)
import Test.Tasty (TestTree, testGroup)

sharedTests :: TestTree
sharedTests = testGroup "Shared tests" [utilsTests]
