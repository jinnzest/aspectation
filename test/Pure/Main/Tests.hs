module Pure.Main.Tests
  ( mainTests,
  )
where

import Pure.Main.Syntax.GeneratorTest (syntaxGeneratorTest)
import Pure.Main.Syntax.ParserErrorTests (syntaxParserErrorTests)
import Pure.Main.Syntax.ParserTests (syntaxParserTests)
import Pure.Main.Syntax.Writer.RangedTests (syntaxRangedWriterTests)
import Pure.Main.Syntax.Writer.SourceTests (syntaxFormattedWriterTests)
import Test.Tasty (TestTree, testGroup)

mainTests :: TestTree
mainTests =
  testGroup
    "Main pure tests"
    [ syntaxRangedWriterTests,
      syntaxParserTests,
      syntaxFormattedWriterTests,
      syntaxParserErrorTests,
      syntaxGeneratorTest
    ]
