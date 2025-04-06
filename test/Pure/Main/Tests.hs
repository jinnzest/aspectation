module Pure.Main.Tests
  ( mainTests,
  )
where

import Pure.Main.Syntax.GeneratorTest (syntaxGeneratorTest)
import Pure.Main.Syntax.ParserErrorTests (syntaxParsingErrorTests)
import Pure.Main.Syntax.ParserTests (syntaxParsingTests)
import Pure.Main.Syntax.Writer.RangedTests (syntaxRangedWriterTests)
import Pure.Main.Syntax.Writer.SourceTests (syntaxFormattedWriterTests)
import Test.Tasty (TestTree, testGroup)

mainTests :: TestTree
mainTests =
  testGroup
    "Main pure tests"
    [ syntaxRangedWriterTests,
      syntaxParsingTests,
      syntaxFormattedWriterTests,
      syntaxParsingErrorTests,
      syntaxGeneratorTest
    ]
