module Pure.Main.Tests
  ( mainTests,
  )
where

import Pure.Main.Syntax.LexerSourceTests (lexerFormattedWritingTests)
import Pure.Main.Syntax.Parsing.LexerErrorTests (lexerErrorTests)
import Pure.Main.Syntax.Parsing.LexerGeneratorTest (lexerGeneratorTest)
import Pure.Main.Syntax.Parsing.LexerTests (lexerTests)
import Pure.Main.Syntax.Writing.LexerRangedTests (lexerRangedWritingTests)
import Test.Tasty (TestTree, testGroup)

mainTests :: TestTree
mainTests =
  testGroup
    "Main pure tests"
    [ lexerRangedWritingTests,
      lexerTests,
      lexerFormattedWritingTests,
      lexerErrorTests,
      lexerGeneratorTest
    ]
