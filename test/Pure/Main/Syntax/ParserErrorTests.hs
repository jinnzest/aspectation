module Pure.Main.Syntax.ParserErrorTests
  ( syntaxParsingErrorTests,
  )
where

import Data.Bool ((&&))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.List (foldr, head, tail)
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Tuple (snd)
import Main.Syntax.Parsing.Parser (syntaxParsing)
import Pure.Main.Syntax.Shared (runParser)
import Shared.Errors (Error (Error), Errors (Errors))
import Shared.Text.Utils (withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (sbt)
import Text.Show (Show (show))

mergeSpaces :: String -> Text
mergeSpaces txt =
  pack $
    snd $
      foldr
        ( \c (lastChar, acc) ->
            if lastChar == ' ' && c == ' '
              then (lastChar, acc)
              else (c, c : acc)
        )
        (head txt, "")
        (tail txt)

assertParsedErrors :: Text -> Text -> Assertion
assertParsedErrors source expected =
  let parsedResult = runParser source (syntaxParsing "")
      expectedText = mergeSpaces $ show (Errors [Error expected])
   in case parsedResult of
        Left actualText ->
          assertEqual "" expectedText (mergeSpaces $ show actualText)
        Right other -> assertEqual "" "an error message" (show other)

testCaseParseError :: Text -> Text -> TestTree
testCaseParseError source expected =
  testCase (unpack $ withBorder source) $ assertParsedErrors source expected

syntaxParsingErrorTests :: TestTree
syntaxParsingErrorTests =
  testGroup
    "Main syntax parser error test"
    [ testCaseParseError
        "nameOnly"
        [sbt|1:9:
            |        |
            |      1 | nameOnly
            |        |         ^
            |      unexpected end of input
            |      expecting "->", '(', a number, a text, an identifier, carriage return, newline, space, or tab
            |],
      testCaseParseError
        " 1"
        [sbt|1:2:
            |        |
            |      1 | 1
            |        | ^
            |      incorrect indentation (got 2, should be equal to 1)
            |],
      testCaseParseError
        "noBodyFunc ="
        [sbt|1:13:
              |        |
              |      1 | noBodyFunc =
              |        |             ^
              |      unexpected end of input
              |      expecting "->", '(', a number, a text, an identifier, carriage return, newline, space, or tab
              |],
      testCaseParseError
        "brokenBodyFunc->("
        [sbt|1:18:
            |        |
            |      1 | brokenBodyFunc->(
            |        |                  ^
            |      unexpected end of input
            |      expecting '(', a number, a text, an identifier, carriage return, newline, space, or tab
            |],
      testCaseParseError
        "f->(123456789"
        [sbt|1:14:
            |        |
            |      1 | f->(123456789
            |        |              ^
            |      unexpected end of input
            |      expecting '(', ')', a number, a text, an identifier, carriage return, newline, space, or tab
            |],
      testCaseParseError
        "f->)"
        [sbt|1:4:
      |        |
      |      1 | f->)
      |        |    ^
      |      unexpected ')'
      |      expecting '(', a number, a text, an identifier, carriage return, newline, space, or tab
      |],
      testCaseParseError
        "+"
        [sbt|1:2:
        |        |
        |      1 | +
        |        |  ^
        |      unexpected end of input
        |      expecting "->", '(', a number, a text, an identifier, carriage return, newline, space, or tab
        |],
      testCaseParseError
        "(xyz->1"
        [sbt|1:5:
        |        |
        |      1 | (xyz->1
        |        |     ^
        |      unexpected '-'
        |      expecting ')', a hash, an identifier, carriage return, newline, space, or tab
        |],
      testCaseParseError
        "->123f"
        [sbt|1:2:
        |        |
        |      1 | ->123f
        |        |  ^
        |      unexpected '>'
        |      expecting a hash, carriage return, integer, newline, space, or tab
        |],
      testCaseParseError
        "#"
        [sbt|1:2:
        |        |
        |      1 | ##
        |        |  ^
        |      unexpected end of input
        |      expecting "->", '(', a number, a text, an identifier, carriage return, newline, space, or tab
        |],
      testCaseParseError
        " x"
        [sbt|1:2:
        |        |
        |      1 |  x
        |        |  ^
        |      incorrect indentation (got 2, should be equal to 1)
        |],
      testCaseParseError
        "f (_ _)"
        [sbt|1:3:
        |        |
        |      1 |  f (_ _)
        |        |    ^^
        |      unexpected "(_"
        |      expecting "->", carriage return, newline, space, or tab
        |],
      testCaseParseError
        "function having an arg (arg) and a text \"text\" in name -> 1"
        [sbt|1:41:
        |        |
        |      1 |  function having an arg (arg) and a text "text" in name -> 1
        |        |                                          ^^
        |      unexpected ""t"
        |      expecting "->", carriage return, newline, space, or tab
        |]
    ]
