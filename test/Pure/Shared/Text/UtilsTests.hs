module Pure.Shared.Text.UtilsTests
  ( utilsTests,
  )
where

import Data.Bool (Bool (False, True))
import Data.Function (($))
import Shared.Text.Utils (hasNL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

utilsTests :: TestTree
utilsTests =
  testGroup
    "Utils tests"
    [ testCase "hasNL = True when there is a new line" $
        let result = hasNL "\t dfsf  \n  dsfsdf \t\t" in assertEqual "" True result,
      testCase "hasNL = True when there is no a new line" $
        let result = hasNL " \t dfsf  \t  dsfsdf \t   " in assertEqual "" False result
    ]
