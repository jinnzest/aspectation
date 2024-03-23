module Pure.Main.Syntax.ParserTests
  ( syntaxParserTests,
  )
where

import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Text (Text)
import Main.Syntax.Parsing.Parser (syntaxParser)
import Main.Syntax.Parsing.Tree (Function)
import Main.Syntax.Writer.Ranged (writeFunctions)
import Pure.Main.Syntax.Shared (runParser, withBorder)
import Shared.Errors (Error (MkError), Errors (MkErrors))
import Shared.Location.Data (Ranged)
import Shared.Text.Utils (nL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)
import Text.Show (Show (show))

alignAR :: [Ranged Function] -> Either a Text
alignAR other = Right [st|#{writeFunctions other}#{nL}|]

assertParsedEqualsSource :: Text -> Text -> Assertion
assertParsedEqualsSource source expected =
  let parsedResult = runParser source syntaxParser
      parsed = case parsedResult of
        Left (MkErrors [MkError error]) -> Left [st|"Error: '#{error}'"|]
        Left errors -> Left [st|"Errors: '#{show errors}'"|]
        Right (other, _) -> alignAR other
   in assertEqual "" (Right expected) parsed

testCaseParse :: Text -> Text -> TestTree
testCaseParse source expected =
  let borderedSource = withBorder source
   in testCase borderedSource $ assertParsedEqualsSource source expected

syntaxParserTests :: TestTree
syntaxParserTests =
  testGroup
    "Main syntax parser test"
    [ testCaseParse
        "func99 -> 2"
        [sbt|
            |1:1 - 1:12 function
            |	function signature
            |		name 1
            |			1:1 - 1:7 alpha numberic word
            |				func99
            |	1:8 - 1:12 function body
            |		1:11 - 1:12 number
            |			integer
            |				2
            |],
      testCaseParse
        "func1 -> 1\nfunc2->2\nfunc3   ->   3  "
        [sbt|
            |1:1 - 1:11 function
            |	function signature
            |		name 1
            |			1:1 - 1:6 alpha numberic word
            |				func1
            |	1:7 - 1:11 function body
            |		1:10 - 1:11 number
            |			integer
            |				1
            |new line
            |2:1 - 2:9 function
            |	function signature
            |		name 1
            |			2:1 - 2:6 alpha numberic word
            |				func2
            |	2:6 - 2:9 function body
            |		2:8 - 2:9 number
            |			integer
            |				2
            |new line
            |3:1 - 3:15 function
            |	function signature
            |		name 1
            |			3:1 - 3:6 alpha numberic word
            |				func3
            |	3:9 - 3:15 function body
            |		3:14 - 3:15 number
            |			integer
            |				3
            |],
      testCaseParse
        "func1 1arg 2arg arg3-> 4arg"
        [sbt|
            |1:1 - 1:28 function
            |	function signature
            |		name 1
            |			1:1 - 1:6 alpha numberic word
            |				func1
            |		argument 1
            |			1:7 - 1:11 alpha numberic word
            |				1arg
            |		argument 2
            |			1:12 - 1:16 alpha numberic word
            |				2arg
            |		argument 3
            |			1:17 - 1:21 alpha numberic word
            |				arg3
            |	1:21 - 1:28 function body
            |		1:24 - 1:28 alpha numberic word
            |			4arg
            |],
      testCaseParse
        "a four words function ->1.2"
        [sbt|
            |1:1 - 1:28 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				a
            |		argument 1
            |			1:3 - 1:7 alpha numberic word
            |				four
            |		argument 2
            |			1:8 - 1:13 alpha numberic word
            |				words
            |		argument 3
            |			1:14 - 1:22 alpha numberic word
            |				function
            |	1:23 - 1:28 function body
            |		1:25 - 1:28 number
            |			integer
            |				1
            |			decimal
            |				2
            |],
      testCaseParse
        "a four words function -> 1.2"
        [sbt|
            |1:1 - 1:29 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				a
            |		argument 1
            |			1:3 - 1:7 alpha numberic word
            |				four
            |		argument 2
            |			1:8 - 1:13 alpha numberic word
            |				words
            |		argument 3
            |			1:14 - 1:22 alpha numberic word
            |				function
            |	1:23 - 1:29 function body
            |		1:26 - 1:29 number
            |			integer
            |				1
            |			decimal
            |				2
            |],
      testCaseParse
        "a mutliwords function with (an argument one) and (an argument two) inside -> \"some text\""
        [sbt|
            |1:1 - 1:89 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				a
            |			1:3 - 1:13 alpha numberic word
            |				mutliwords
            |			1:14 - 1:22 alpha numberic word
            |				function
            |			1:23 - 1:27 alpha numberic word
            |				with
            |		argument 1
            |			argument in brackets 1
            |				1:29 - 1:31 alpha numberic word
            |					an
            |				1:32 - 1:40 alpha numberic word
            |					argument
            |				1:41 - 1:44 alpha numberic word
            |					one
            |		name 2
            |			1:46 - 1:49 alpha numberic word
            |				and
            |		argument 2
            |			argument in brackets 2
            |				1:51 - 1:53 alpha numberic word
            |					an
            |				1:54 - 1:62 alpha numberic word
            |					argument
            |				1:63 - 1:66 alpha numberic word
            |					two
            |		name 3
            |			1:68 - 1:74 alpha numberic word
            |				inside
            |	1:75 - 1:89 function body
            |		1:78 - 1:89 text
            |			some text
            |],
      testCaseParse
        "(first arg) = (second arg) -> #*:"
        [sbt|
            |1:1 - 1:34 function
            |	function signature
            |		argument 1
            |			argument in brackets 1
            |				1:2 - 1:7 alpha numberic word
            |					first
            |				1:8 - 1:11 alpha numberic word
            |					arg
            |		name 1
            |			1:13 - 1:14 non alpha numberic word
            |				=
            |		argument 2
            |			argument in brackets 2
            |				1:16 - 1:22 alpha numberic word
            |					second
            |				1:23 - 1:26 alpha numberic word
            |					arg
            |	1:28 - 1:34 function body
            |		1:31 - 1:34 non alpha numberic word
            |			#*:
            |],
      testCaseParse
        "func->expr1 (nested expr) expr2"
        [sbt|
            |1:1 - 1:32 function
            |	function signature
            |		name 1
            |			1:1 - 1:5 alpha numberic word
            |				func
            |	1:5 - 1:32 function body
            |		1:7 - 1:12 alpha numberic word
            |			expr1
            |		1:13 - 1:26 higher priority expressions
            |			1:14 - 1:20 alpha numberic word
            |				nested
            |			1:21 - 1:25 alpha numberic word
            |				expr
            |		1:27 - 1:32 alpha numberic word
            |			expr2
            |],
      testCaseParse
        "func15->\n\texpr1\n\texpr2"
        [sbt|
            |1:1 - 3:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:7 alpha numberic word
            |				func15
            |	1:7 - 3:14 function body
            |		new line
            |		2:9 - 2:14 alpha numberic word
            |			expr1
            |		new line
            |		3:9 - 3:14 alpha numberic word
            |			expr2
            |],
      testCaseParse
        "func->\n\texpr1\n\t1.2\n\t\tnested expression1\n\t\tnested expression2\n\texpr2\n\t\tnested expression3\n\t\tnested expression4\n\texpr3\n\ta->b"
        [sbt|
            |1:1 - 10:13 function
            |	function signature
            |		name 1
            |			1:1 - 1:5 alpha numberic word
            |				func
            |	1:5 - 10:13 function body
            |		new line
            |		2:9 - 2:14 alpha numberic word
            |			expr1
            |		new line
            |		3:9 - 3:12 number
            |			integer
            |				1
            |			decimal
            |				2
            |		nested expressions
            |				new line
            |				4:17 - 4:23 alpha numberic word
            |					nested
            |				4:24 - 4:35 alpha numberic word
            |					expression1
            |				new line
            |				5:17 - 5:23 alpha numberic word
            |					nested
            |				5:24 - 5:35 alpha numberic word
            |					expression2
            |		new line
            |		6:9 - 6:14 alpha numberic word
            |			expr2
            |		nested expressions
            |				new line
            |				7:17 - 7:23 alpha numberic word
            |					nested
            |				7:24 - 7:35 alpha numberic word
            |					expression3
            |				new line
            |				8:17 - 8:23 alpha numberic word
            |					nested
            |				8:24 - 8:35 alpha numberic word
            |					expression4
            |		new line
            |		9:9 - 9:14 alpha numberic word
            |			expr3
            |		new line
            |		10:9 - 10:10 alpha numberic word
            |			a
            |		10:10 - 10:12 non alpha numberic word
            |			->
            |		10:12 - 10:13 alpha numberic word
            |			b
            |],
      testCaseParse
        "func->\n\texpr1\n\t\tnested expression 1\n\t\tnested expression 2\n\texpr2 expr3\n\texpr4"
        [sbt|
            |1:1 - 6:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:5 alpha numberic word
            |				func
            |	1:5 - 6:14 function body
            |		new line
            |		2:9 - 2:14 alpha numberic word
            |			expr1
            |		nested expressions
            |				new line
            |				3:17 - 3:23 alpha numberic word
            |					nested
            |				3:24 - 3:34 alpha numberic word
            |					expression
            |				3:35 - 3:36 number
            |					integer
            |						1
            |				new line
            |				4:17 - 4:23 alpha numberic word
            |					nested
            |				4:24 - 4:34 alpha numberic word
            |					expression
            |				4:35 - 4:36 number
            |					integer
            |						2
            |		new line
            |		5:9 - 5:14 alpha numberic word
            |			expr2
            |		5:15 - 5:20 alpha numberic word
            |			expr3
            |		new line
            |		6:9 - 6:14 alpha numberic word
            |			expr4
            |],
      testCaseParse
        "func->id1+++id2"
        [sbt|
            |1:1 - 1:16 function
            |	function signature
            |		name 1
            |			1:1 - 1:5 alpha numberic word
            |				func
            |	1:5 - 1:16 function body
            |		1:7 - 1:10 alpha numberic word
            |			id1
            |		1:10 - 1:13 non alpha numberic word
            |			+++
            |		1:13 - 1:16 alpha numberic word
            |			id2
            |],
      testCaseParse
        "f -> (xV7 ...YL MG 68185085017189.7621760004214).CEp dsf"
        [sbt|
            |1:1 - 1:57 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				f
            |	1:3 - 1:57 function body
            |		1:6 - 1:49 higher priority expressions
            |			1:7 - 1:10 alpha numberic word
            |				xV7
            |			1:11 - 1:14 non alpha numberic word
            |				...
            |			1:14 - 1:16 alpha numberic word
            |				YL
            |			1:17 - 1:19 alpha numberic word
            |				MG
            |			1:20 - 1:48 number
            |				integer
            |					68185085017189
            |				decimal
            |					7621760004214
            |		1:49 - 1:50 non alpha numberic word
            |			.
            |		1:50 - 1:53 alpha numberic word
            |			CEp
            |		1:54 - 1:57 alpha numberic word
            |			dsf
            |],
      testCaseParse
        "nesting1 -> \n\t1\n\t\t2\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting1
            |	1:10 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting2 -> \n\t1\n\t\t2 3\n\t4"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting2
            |	1:10 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				3:19 - 3:20 number
            |					integer
            |						3
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				4
            |],
      testCaseParse
        "nesting3 -> \n\t1\n\t\t2 3 4\n\t5"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting3
            |	1:10 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				3:19 - 3:20 number
            |					integer
            |						3
            |				3:21 - 3:22 number
            |					integer
            |						4
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting4 -> \n\t1\n\t\t\t\t\t\t2\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting4
            |	1:10 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:49 - 3:50 number
            |					integer
            |						2
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting5 -> \n\t1\n\t\t2 3\n\t\t4 5\n\t6"
        [sbt|
            |1:1 - 5:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting5
            |	1:10 - 5:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				3:19 - 3:20 number
            |					integer
            |						3
            |				new line
            |				4:17 - 4:18 number
            |					integer
            |						4
            |				4:19 - 4:20 number
            |					integer
            |						5
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				6
            |],
      testCaseParse
        "nesting6 -> \n\t1\n\t\t2 3 4\n\t\t5 6 7\n\t8"
        [sbt|
            |1:1 - 5:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting6
            |	1:10 - 5:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				3:19 - 3:20 number
            |					integer
            |						3
            |				3:21 - 3:22 number
            |					integer
            |						4
            |				new line
            |				4:17 - 4:18 number
            |					integer
            |						5
            |				4:19 - 4:20 number
            |					integer
            |						6
            |				4:21 - 4:22 number
            |					integer
            |						7
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				8
            |],
      testCaseParse
        "nesting7 -> \n\t1\n\t\t2\n\t3\n\t\t4\n\t5\n\t\t6\n\t7"
        [sbt|
            |1:1 - 8:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting7
            |	1:10 - 8:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				3
            |		nested expressions
            |				new line
            |				5:17 - 5:18 number
            |					integer
            |						4
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				5
            |		nested expressions
            |				new line
            |				7:17 - 7:18 number
            |					integer
            |						6
            |		new line
            |		8:9 - 8:10 number
            |			integer
            |				7
            |],
      testCaseParse
        "nesting8 -> \n\t1\n\t\t2 3 4\n\t5\n\t\t6 7 8\n\t9\n\t\t10 11 12\n\t13"
        [sbt|
            |1:1 - 8:11 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting8
            |	1:10 - 8:11 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				3:19 - 3:20 number
            |					integer
            |						3
            |				3:21 - 3:22 number
            |					integer
            |						4
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				5
            |		nested expressions
            |				new line
            |				5:17 - 5:18 number
            |					integer
            |						6
            |				5:19 - 5:20 number
            |					integer
            |						7
            |				5:21 - 5:22 number
            |					integer
            |						8
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				9
            |		nested expressions
            |				new line
            |				7:17 - 7:19 number
            |					integer
            |						10
            |				7:20 - 7:22 number
            |					integer
            |						11
            |				7:23 - 7:25 number
            |					integer
            |						12
            |		new line
            |		8:9 - 8:11 number
            |			integer
            |				13
            |],
      testCaseParse
        "nesting9 -> \n\t1\n\t\t2\n\t\t\t3\n\t\t\t\t4\n\t\t\t\t\t5"
        [sbt|
            |1:1 - 6:42 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				nesting9
            |	1:10 - 6:42 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						2
            |				nested expressions
            |						new line
            |						4:25 - 4:26 number
            |							integer
            |								3
            |						nested expressions
            |								new line
            |								5:33 - 5:34 number
            |									integer
            |										4
            |								nested expressions
            |										new line
            |										6:41 - 6:42 number
            |											integer
            |												5
            |],
      testCaseParse
        "nesting10 -> \n\t\t\t\t\t1\n\t\t\t\t2\n\t\t\t3\n\t\t4\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting10
            |	1:11 - 6:10 function body
            |		nested expressions
            |				nested expressions
            |						nested expressions
            |								nested expressions
            |										new line
            |										2:41 - 2:42 number
            |											integer
            |												1
            |								new line
            |								3:33 - 3:34 number
            |									integer
            |										2
            |						new line
            |						4:25 - 4:26 number
            |							integer
            |								3
            |				new line
            |				5:17 - 5:18 number
            |					integer
            |						4
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting11 -> \n\t1\n\t\t\t2\n\t\t3\n\t\t\t\t4\n\t\t\t5\n\t6"
        [sbt|
            |1:1 - 7:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting11
            |	1:11 - 7:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				nested expressions
            |						new line
            |						3:25 - 3:26 number
            |							integer
            |								2
            |				new line
            |				4:17 - 4:18 number
            |					integer
            |						3
            |				nested expressions
            |						nested expressions
            |								new line
            |								5:33 - 5:34 number
            |									integer
            |										4
            |						new line
            |						6:25 - 6:26 number
            |							integer
            |								5
            |		new line
            |		7:9 - 7:10 number
            |			integer
            |				6
            |],
      testCaseParse
        "nesting12 -> \n\t1(2)3"
        [sbt|
            |1:1 - 2:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting12
            |	1:11 - 2:14 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 2:13 higher priority expressions
            |			2:11 - 2:12 number
            |				integer
            |					2
            |		2:13 - 2:14 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting13 -> \n\t1\n\t(2)3"
        [sbt|
            |1:1 - 3:13 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting13
            |	1:11 - 3:13 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		new line
            |		3:9 - 3:12 higher priority expressions
            |			3:10 - 3:11 number
            |				integer
            |					2
            |		3:12 - 3:13 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting14 -> \n\t1(\n\t2)3"
        [sbt|
            |1:1 - 3:12 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting14
            |	1:11 - 3:12 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 3:11 higher priority expressions
            |			new line
            |			3:9 - 3:10 number
            |				integer
            |					2
            |		3:11 - 3:12 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting15 -> \n\t1(2\n\t)3"
        [sbt|
            |1:1 - 3:11 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting15
            |	1:11 - 3:11 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 3:10 higher priority expressions
            |			2:11 - 2:12 number
            |				integer
            |					2
            |			new line
            |		3:10 - 3:11 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting16 -> \n\t1(2)\n\t3"
        [sbt|
            |1:1 - 3:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting16
            |	1:11 - 3:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 2:13 higher priority expressions
            |			2:11 - 2:12 number
            |				integer
            |					2
            |		new line
            |		3:9 - 3:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting17 -> \n\t1\n\t(2)\n\t3"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting17
            |	1:11 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		new line
            |		3:9 - 3:12 higher priority expressions
            |			3:10 - 3:11 number
            |				integer
            |					2
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting18 -> \n\t1\n\t(\n\t2)\n\t3"
        [sbt|
            |1:1 - 5:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting18
            |	1:11 - 5:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		new line
            |		3:9 - 4:11 higher priority expressions
            |			new line
            |			4:9 - 4:10 number
            |				integer
            |					2
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting19 -> \n\t1\n\t(\n\t2\n\t)\n\t3"
        [sbt|
            |1:1 - 6:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting19
            |	1:11 - 6:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		new line
            |		3:9 - 5:10 higher priority expressions
            |			new line
            |			4:9 - 4:10 number
            |				integer
            |					2
            |			new line
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				3
            |],
      testCaseParse
        "nesting20 -> \n\t1(2\n\t3)\n\t4"
        [sbt|
            |1:1 - 4:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting20
            |	1:11 - 4:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 3:11 higher priority expressions
            |			2:11 - 2:12 number
            |				integer
            |					2
            |			nested expressions
            |					new line
            |					3:9 - 3:10 number
            |						integer
            |							3
            |		new line
            |		4:9 - 4:10 number
            |			integer
            |				4
            |],
      testCaseParse
        "nesting21 -> \n\t1(2\n\t\t3\n\t4)\n\t5"
        [sbt|
            |1:1 - 5:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting21
            |	1:11 - 5:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 4:11 higher priority expressions
            |			2:11 - 2:12 number
            |				integer
            |					2
            |			nested expressions
            |					nested expressions
            |							new line
            |							3:17 - 3:18 number
            |								integer
            |									3
            |					new line
            |					4:9 - 4:10 number
            |						integer
            |							4
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting22 -> \n\t1(\n\t\t2\n\t3\n\t4)\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting22
            |	1:11 - 6:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 5:11 higher priority expressions
            |			nested expressions
            |					new line
            |					3:17 - 3:18 number
            |						integer
            |							2
            |			new line
            |			4:9 - 4:10 number
            |				integer
            |					3
            |			new line
            |			5:9 - 5:10 number
            |				integer
            |					4
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting23 -> \n\t1(\n\t2\n\t3\n\t\t4)\n\t5"
        [sbt|
            |1:1 - 6:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting23
            |	1:11 - 6:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 5:19 higher priority expressions
            |			new line
            |			3:9 - 3:10 number
            |				integer
            |					2
            |			new line
            |			4:9 - 4:10 number
            |				integer
            |					3
            |			nested expressions
            |					new line
            |					5:17 - 5:18 number
            |						integer
            |							4
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting24 -> \n\t1\n\t\t(\n\t\t2\n\t\t3\n\t\t4)\n\t5"
        [sbt|
            |1:1 - 7:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting24
            |	1:11 - 7:10 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				3:17 - 6:19 higher priority expressions
            |					new line
            |					4:17 - 4:18 number
            |						integer
            |							2
            |					new line
            |					5:17 - 5:18 number
            |						integer
            |							3
            |					new line
            |					6:17 - 6:18 number
            |						integer
            |							4
            |		new line
            |		7:9 - 7:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting25 -> \n\t\t(\n\t\t1\n\t\t2\n\t\t3)\n\t4\n\t5"
        [sbt|
            |1:1 - 7:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting25
            |	1:11 - 7:10 function body
            |		nested expressions
            |				new line
            |				2:17 - 5:19 higher priority expressions
            |					new line
            |					3:17 - 3:18 number
            |						integer
            |							1
            |					new line
            |					4:17 - 4:18 number
            |						integer
            |							2
            |					new line
            |					5:17 - 5:18 number
            |						integer
            |							3
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				4
            |		new line
            |		7:9 - 7:10 number
            |			integer
            |				5
            |],
      testCaseParse
        "nesting26 -> \n\t1\n\t2(\n\t\t3\n\t\t4\n\t\t5)"
        [sbt|
            |1:1 - 6:19 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting26
            |	1:11 - 6:19 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		new line
            |		3:9 - 3:10 number
            |			integer
            |				2
            |		3:10 - 6:19 higher priority expressions
            |			new line
            |			4:17 - 4:18 number
            |				integer
            |					3
            |			new line
            |			5:17 - 5:18 number
            |				integer
            |					4
            |			new line
            |			6:17 - 6:18 number
            |				integer
            |					5
            |],
      testCaseParse
        "nesting27 -> \n\t1(\n\t\t2 3 4\n\t\t5 6 7\n\t\t8 9 10)\n\t11"
        [sbt|
            |1:1 - 6:11 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting27
            |	1:11 - 6:11 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:10 - 5:24 higher priority expressions
            |			new line
            |			3:17 - 3:18 number
            |				integer
            |					2
            |			3:19 - 3:20 number
            |				integer
            |					3
            |			3:21 - 3:22 number
            |				integer
            |					4
            |			new line
            |			4:17 - 4:18 number
            |				integer
            |					5
            |			4:19 - 4:20 number
            |				integer
            |					6
            |			4:21 - 4:22 number
            |				integer
            |					7
            |			new line
            |			5:17 - 5:18 number
            |				integer
            |					8
            |			5:19 - 5:20 number
            |				integer
            |					9
            |			5:21 - 5:23 number
            |				integer
            |					10
            |		new line
            |		6:9 - 6:11 number
            |			integer
            |				11
            |],
      testCaseParse
        "nesting28 -> \n\t1 2 3(\n\t\t3 4.8e7 5\n\t\t)\n\t6 7 8"
        [sbt|
            |1:1 - 5:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting28
            |	1:11 - 5:14 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:11 - 2:12 number
            |			integer
            |				2
            |		2:13 - 2:14 number
            |			integer
            |				3
            |		2:14 - 4:18 higher priority expressions
            |			new line
            |			3:17 - 3:18 number
            |				integer
            |					3
            |			3:19 - 3:24 number
            |				integer
            |					4
            |				decimal
            |					8
            |				exponent
            |					7
            |			3:25 - 3:26 number
            |				integer
            |					5
            |			new line
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				6
            |		5:11 - 5:12 number
            |			integer
            |				7
            |		5:13 - 5:14 number
            |			integer
            |				8
            |],
      testCaseParse
        "nesting29 -> \n\t1 2 3\n\t(\n\t\t3 4 5\n\t\t)\n\t6 7 8"
        [sbt|
            |1:1 - 6:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting29
            |	1:11 - 6:14 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:11 - 2:12 number
            |			integer
            |				2
            |		2:13 - 2:14 number
            |			integer
            |				3
            |		new line
            |		3:9 - 5:18 higher priority expressions
            |			new line
            |			4:17 - 4:18 number
            |				integer
            |					3
            |			4:19 - 4:20 number
            |				integer
            |					4
            |			4:21 - 4:22 number
            |				integer
            |					5
            |			new line
            |		new line
            |		6:9 - 6:10 number
            |			integer
            |				6
            |		6:11 - 6:12 number
            |			integer
            |				7
            |		6:13 - 6:14 number
            |			integer
            |				8
            |],
      testCaseParse
        "nesting30 ->\n\t\t1\n\t(2)"
        [sbt|
            |1:1 - 3:12 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting30
            |	1:11 - 3:12 function body
            |		nested expressions
            |				new line
            |				2:17 - 2:18 number
            |					integer
            |						1
            |		new line
            |		3:9 - 3:12 higher priority expressions
            |			3:10 - 3:11 number
            |				integer
            |					2
            |],
      testCaseParse
        "nesting31 ->\n\t\t1\n\t\t\t2.3\n\t\t\t(\t\t\t3)\n\t4"
        [sbt|
            |1:1 - 5:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting31
            |	1:11 - 5:10 function body
            |		nested expressions
            |				new line
            |				2:17 - 2:18 number
            |					integer
            |						1
            |				nested expressions
            |						new line
            |						3:25 - 3:28 number
            |							integer
            |								2
            |							decimal
            |								3
            |						new line
            |						4:25 - 4:51 higher priority expressions
            |							4:49 - 4:50 number
            |								integer
            |									3
            |		new line
            |		5:9 - 5:10 number
            |			integer
            |				4
            |],
      testCaseParse
        "nesting32 ->\n\t1 \t\t\t(\n\t\t\t(\n\t\t\t4e6\n \t\t\t\t5))"
        [sbt|
            |1:1 - 5:36 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting32
            |	1:11 - 5:36 function body
            |		new line
            |		2:9 - 2:10 number
            |			integer
            |				1
            |		2:33 - 5:36 higher priority expressions
            |			new line
            |			3:25 - 5:35 higher priority expressions
            |				new line
            |				4:25 - 4:28 number
            |					integer
            |						4
            |					exponent
            |						6
            |				new line
            |				5:33 - 5:34 number
            |					integer
            |						5
            |],
      testCaseParse
        "nesting33 -> 1\n\t2\n\t3"
        [sbt|
            |1:1 - 3:10 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting33
            |	1:11 - 3:10 function body
            |		1:14 - 1:15 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				2:9 - 2:10 number
            |					integer
            |						2
            |				new line
            |				3:9 - 3:10 number
            |					integer
            |						3
            |],
      testCaseParse
        "nesting34 -> 1\n\t2\n\t\t3"
        [sbt|
            |1:1 - 3:18 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting34
            |	1:11 - 3:18 function body
            |		1:14 - 1:15 number
            |			integer
            |				1
            |		nested expressions
            |				new line
            |				2:9 - 2:10 number
            |					integer
            |						2
            |				nested expressions
            |						new line
            |						3:17 - 3:18 number
            |							integer
            |								3
            |],
      testCaseParse
        "nesting35\n\targ-> 1 2\n\t\t3"
        [sbt|
            |1:1 - 3:18 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting35
            |		argument 1
            |			new line
            |			2:9 - 2:12 alpha numberic word
            |				arg
            |	2:12 - 3:18 function body
            |		2:15 - 2:16 number
            |			integer
            |				1
            |		2:17 - 2:18 number
            |			integer
            |				2
            |		nested expressions
            |				new line
            |				3:17 - 3:18 number
            |					integer
            |						3
            |],
      testCaseParse
        "nesting36\n\targ->\n\t(1\n\t)\t2"
        [sbt|
            |1:1 - 4:18 function
            |	function signature
            |		name 1
            |			1:1 - 1:10 alpha numberic word
            |				nesting36
            |		argument 1
            |			new line
            |			2:9 - 2:12 alpha numberic word
            |				arg
            |	2:12 - 4:18 function body
            |		new line
            |		3:9 - 4:10 higher priority expressions
            |			3:10 - 3:11 number
            |				integer
            |					1
            |			new line
            |		4:17 - 4:18 number
            |			integer
            |				2
            |],
      testCaseParse
        "a\n\tfunctioN()\n\t -> 1"
        [sbt|
            |1:1 - 3:14 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				a
            |			new line
            |			2:9 - 2:17 alpha numberic word
            |				functioN
            |		argument 1
            |			empty argument
            |	new line
            |	3:10 - 3:14 function body
            |		3:13 - 3:14 number
            |			integer
            |				1
            |],
      testCaseParse
        "function\n\targ1\n\targ2 -> arg1"
        [sbt|
            |1:1 - 3:21 function
            |	function signature
            |		name 1
            |			1:1 - 1:9 alpha numberic word
            |				function
            |		argument 1
            |			new line
            |			2:9 - 2:13 alpha numberic word
            |				arg1
            |		argument 2
            |			new line
            |			3:9 - 3:13 alpha numberic word
            |				arg2
            |	3:14 - 3:21 function body
            |		3:17 - 3:21 alpha numberic word
            |			arg1
            |],
      testCaseParse
        "x*-abs%db:::-:xy->x"
        [sbt|
            |1:1 - 1:20 function
            |	function signature
            |		name 1
            |			1:1 - 1:2 alpha numberic word
            |				x
            |		argument 1
            |			1:2 - 1:4 non alpha numberic word
            |				*-
            |		argument 2
            |			1:4 - 1:7 alpha numberic word
            |				abs
            |		argument 3
            |			1:7 - 1:8 non alpha numberic word
            |				%
            |		argument 4
            |			1:8 - 1:10 alpha numberic word
            |				db
            |		argument 5
            |			1:10 - 1:15 non alpha numberic word
            |				:::-:
            |		argument 6
            |			1:15 - 1:17 alpha numberic word
            |				xy
            |	1:17 - 1:20 function body
            |		1:19 - 1:20 alpha numberic word
            |			x
            |],
      testCaseParse
        "number ->  1e-1 -1e1 0e1"
        [sbt|
            |1:1 - 1:25 function
            |	function signature
            |		name 1
            |			1:1 - 1:7 alpha numberic word
            |				number
            |	1:8 - 1:25 function body
            |		1:12 - 1:16 number
            |			integer
            |				1
            |			exponent
            |				-1
            |		1:17 - 1:21 number
            |			integer
            |				-1
            |			exponent
            |				1
            |		1:22 - 1:25 number
            |			integer
            |				0
            |			exponent
            |				1
            |],
      testCaseParse
        "some func ->  \"beginning \"\"then some text inside\"\" and  \"\"\"\"again\"\"\"\"  and then continuation\" "
        [sbt|
            |1:1 - 1:94 function
            |	function signature
            |		name 1
            |			1:1 - 1:5 alpha numberic word
            |				some
            |		argument 1
            |			1:6 - 1:10 alpha numberic word
            |				func
            |	1:11 - 1:94 function body
            |		1:15 - 1:94 text
            |			beginning "then some text inside" and  ""again""  and then continuation
            |]
    ]
