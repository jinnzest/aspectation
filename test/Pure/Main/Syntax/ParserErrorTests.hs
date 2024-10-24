module Pure.Main.Syntax.ParserErrorTests
  ( syntaxParserErrorTests,
  )
where

import Data.Bool ((&&))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.List (foldr, head, tail)
import Data.String (String)
import Data.Text (Text, pack)
import Data.Tuple (snd)
import Main.Syntax.Parsing.Parser (syntaxParser)
import Pure.Main.Syntax.Shared (runParser, withBorder)
import Shared.Errors (Error (MkError), Errors (MkErrors))
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
  let parsedResult = runParser source syntaxParser
      expectedText = mergeSpaces $ show (MkErrors [MkError expected])
   in case parsedResult of
        Left actualText ->
          assertEqual "" expectedText (mergeSpaces $ show actualText)
        Right other -> assertEqual "" "an error message" (show other)

testCaseParseError :: Text -> Text -> TestTree
testCaseParseError source expected =
  testCase (withBorder source) $ assertParsedErrors source expected

syntaxParserErrorTests :: TestTree
syntaxParserErrorTests =
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
            |      expecting '(', ')', a number, a text, an identifier, carriage return, newline, space, or tab
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
        |      expecting ')', an identifier, carriage return, newline, space, or tab
        |],
      testCaseParseError
        "->123f"
        [sbt|1:2:
        |        |
        |      1 | ->123f
        |        |  ^
        |      unexpected '>'
        |      expecting carriage return, integer, newline, space, or tab
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
        |]
    ]
