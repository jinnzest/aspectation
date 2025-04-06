module Pure.Main.Syntax.Writer.SourceTests
  ( syntaxFormattedWriterTests,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Text (Text, unpack)
import Main.Syntax.Parsing.Parser (syntaxParsing)
import Main.Syntax.Writer.Source (writeFunctions)
import Pure.Main.Syntax.Shared (runParser)
import Shared.Errors (Error (Error), Errors (Errors))
import Shared.Text.Utils (withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (st)
import Text.Show (Show (show))

assertWrittenParsedEqualsSource :: Text -> Assertion
assertWrittenParsedEqualsSource source =
  let parsed = runParser source (syntaxParsing "")
      written = case parsed of
        Left (Errors [Error error]) -> Left [st|"Error: '#{error}'"|]
        Left errors -> Left [st|"Errors: '#{show errors}'"|]
        Right (other, _) -> Right $ writeFunctions other
   in assertEqual "" (Right source) written

testCaseParseFormatted :: Text -> TestTree
testCaseParseFormatted source =
  testCase (unpack $ withBorder source) $ assertWrittenParsedEqualsSource source

syntaxFormattedWriterTests :: TestTree
syntaxFormattedWriterTests =
  testGroup
    "Syntax formatted writer tests"
    [ testCaseParseFormatted "functionName\t->\tid",
      testCaseParseFormatted "functionName argument\t->\t5",
      testCaseParseFormatted "function1 (argument) name\t->\t6.7",
      testCaseParseFormatted "function2 with (argument one) and (argument two) between\t->\t\"some text\"",
      testCaseParseFormatted "#(argument one) -> (argument two)\t->\t*:",
      testCaseParseFormatted "function3\t->\t->",
      testCaseParseFormatted "function4\t->\n\t1\t2\t3.4\t\"some text\"",
      testCaseParseFormatted "function5\t->\n\t1\t2\n\t\tsub1\t\tsub2\n\t3.4\t\"some text\"",
      testCaseParseFormatted "function6\t->\t...\tsome\tindex\ttype",
      testCaseParseFormatted "func1\t->\ta1\nfunc2\t->\n\t1\t2\nfunc3\t->\tf3",
      testCaseParseFormatted "func1\t->\n\ta1\ta2\nfunc2\t->\n\t3234\tsdfsdf\nfunc3\t->\n\tf3\te3",
      testCaseParseFormatted "# some -> some",
      testCaseParseFormatted "# some -> one | another",
      testCaseParseFormatted "# some -> \n\tone | \n\tanother",
      testCaseParseFormatted "# \n\tsome\n\tdata\n\ttype -> \n\tone | \n\tanother",
      testCaseParseFormatted "func (_ ) -> 1"
    ]
