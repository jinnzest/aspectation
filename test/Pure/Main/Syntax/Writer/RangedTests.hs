module Pure.Main.Syntax.Writer.RangedTests
  ( syntaxRangedWriterTests,
  )
where

import Data.Function (($))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text, unpack)
import Main.Syntax.Parsing.Tree
  ( ExprsBlock (ExprsBlock),
    Function (Function, fBody, fSignature),
    FunctionBody (FunctionBody),
    FunctionSignature (FunctionSignature, fsItems, fsURL),
    FunctionSignatureItem (FunctionArgument, FunctionName),
    Number (Number, dec, exp, int),
    TokenExpr
      ( AlphaNumExpr,
        HigherPriorityExpr,
        NestedExpr,
        NumberExpr,
        TextExpr
      ),
  )
import Main.Syntax.Writer.Ranged (writeFunctions)
import Pure.Main.Syntax.Shared (mkOcRanged, mkRanged)
import Shared.Location.Data
  ( Ranged,
  )
import Shared.Text.Utils (nL, withBorder)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Shakespeare.Text (sbt, st)

alignAR :: [Ranged Function] -> Text
alignAR other = [st|#{writeFunctions other}#{nL}|]

testCaseParse :: Text -> [Ranged Function] -> Text -> TestTree
testCaseParse name source expected = testCase (unpack $ withBorder name) $ assertEqual "" expected (alignAR source)

syntaxRangedWriterTests :: TestTree
syntaxRangedWriterTests =
  testGroup
    "Syntax ranged writer tests"
    [ testCaseParse
        "write a function without args which returns an identifier"
        [ mkRanged 1 2 7 8 $
            Function
              { fSignature = FunctionSignature {fsItems = [FunctionName $ AlphaNumExpr $ mkRanged 1 2 3 4 "Name"], fsURL = ""},
                fBody = FunctionBody $ mkRanged 5 6 7 8 [ExprsBlock [AlphaNumExpr $ mkRanged 5 6 7 8 "id"]]
              }
        ]
        [sbt|
            |1:2 - 7:8 function
            |	1:2 - 3:4 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name
            |	5:6 - 7:8 function body
            |		5:6 - 7:8 expressions block
            |			5:6 - 7:8 alpha numberic word
            |				id
            |],
      testCaseParse
        "write a function which take an argument and returns an alpha num identifier number"
        [ mkRanged 1 2 11 12 $
            Function
              { fSignature =
                  FunctionSignature
                    { fsItems =
                        [ FunctionName $ AlphaNumExpr $ mkRanged 1 2 3 4 "Name",
                          FunctionArgument $ AlphaNumExpr $ mkRanged 5 6 7 8 "Arg"
                        ],
                      fsURL = ""
                    },
                fBody = FunctionBody $ mkRanged 13 14 15 16 [ExprsBlock [AlphaNumExpr $ mkRanged 9 10 11 12 "1"]]
              }
        ]
        [sbt|
            |1:2 - 11:12 function
            |	1:2 - 7:8 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name
            |		5:6 - 7:8 argument 1
            |			5:6 - 7:8 alpha numberic word
            |				Arg
            |	13:14 - 15:16 function body
            |		9:10 - 11:12 expressions block
            |			9:10 - 11:12 alpha numberic word
            |				1
            |],
      testCaseParse
        "write a function with a multi-word name and a multi-word argument that returns a text"
        [ mkRanged 1 2 23 24 $
            Function
              { fSignature =
                  FunctionSignature
                    { fsItems =
                        [ FunctionName $
                            AlphaNumExpr $
                              mkRanged 1 2 3 4 "Name1",
                          FunctionName $ AlphaNumExpr $ mkRanged 5 6 7 8 "Name2",
                          FunctionArgument $ AlphaNumExpr $ mkRanged 9 10 11 12 "Arg1",
                          FunctionArgument $ AlphaNumExpr $ mkRanged 13 14 15 16 "Arg2"
                        ],
                      fsURL = ""
                    },
                fBody = FunctionBody $ mkRanged 25 26 27 28 [ExprsBlock [TextExpr $ mkRanged 21 22 23 24 "text"]]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 15:16 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name1
            |		5:6 - 7:8 name 2
            |			5:6 - 7:8 alpha numberic word
            |				Name2
            |		9:10 - 11:12 argument 1
            |			9:10 - 11:12 alpha numberic word
            |				Arg1
            |		13:14 - 15:16 argument 2
            |			13:14 - 15:16 alpha numberic word
            |				Arg2
            |	25:26 - 27:28 function body
            |		21:22 - 23:24 expressions block
            |			21:22 - 23:24 text
            |				text
            |],
      testCaseParse
        "write a function with two arguments having a multi-lined body of different expression types"
        [ mkRanged 1 2 27 28 $
            Function
              { fSignature =
                  FunctionSignature
                    { fsItems =
                        [ FunctionName $ AlphaNumExpr $ mkRanged 1 2 3 4 "Name",
                          FunctionArgument $ AlphaNumExpr $ mkRanged 5 6 7 8 "Arg1",
                          FunctionArgument $ AlphaNumExpr $ mkRanged 9 10 11 12 "Arg2"
                        ],
                      fsURL = ""
                    },
                fBody =
                  FunctionBody $
                    mkRanged
                      29
                      30
                      31
                      32
                      [ ExprsBlock
                          [ AlphaNumExpr $ mkRanged 13 14 15 16 "id",
                            AlphaNumExpr $ mkRanged 17 18 19 20 "1",
                            NumberExpr $ mkRanged 21 22 23 24 $ Number {int = 1, dec = Just 2, exp = Nothing},
                            TextExpr $ mkRanged 25 26 27 28 "the\tfirst line of a text\nthe second line of a text"
                          ]
                      ]
              }
        ]
        [sbt|
            |1:2 - 27:28 function
            |	1:2 - 11:12 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name
            |		5:6 - 7:8 argument 1
            |			5:6 - 7:8 alpha numberic word
            |				Arg1
            |		9:10 - 11:12 argument 2
            |			9:10 - 11:12 alpha numberic word
            |				Arg2
            |	29:30 - 31:32 function body
            |		13:14 - 27:28 expressions block
            |			13:14 - 15:16 alpha numberic word
            |				id
            |			17:18 - 19:20 alpha numberic word
            |				1
            |			21:22 - 23:24 number
            |				integer
            |					1
            |				decimal
            |					2
            |			25:26 - 27:28 text
            |				the	first line of a text
            |the second line of a text
            |],
      testCaseParse
        "write a function without args which contains nested expressions"
        [ mkRanged 1 2 23 24 $
            Function
              { fSignature = FunctionSignature {fsItems = [FunctionName $ AlphaNumExpr $ mkRanged 1 2 3 4 "Name"], fsURL = ""},
                fBody =
                  FunctionBody $
                    mkRanged
                      22
                      23
                      24
                      25
                      [ ExprsBlock
                          [ AlphaNumExpr $ mkRanged 5 6 7 8 "id",
                            NestedExpr
                              [ ExprsBlock
                                  [ AlphaNumExpr $ mkRanged 13 14 15 16 "subId",
                                    AlphaNumExpr $ mkRanged 17 18 19 20 "subId2"
                                  ]
                              ],
                            AlphaNumExpr $ mkRanged 21 22 23 24 "id2"
                          ]
                      ]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 3:4 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name
            |	22:23 - 24:25 function body
            |		5:6 - 23:24 expressions block
            |			5:6 - 7:8 alpha numberic word
            |				id
            |			13:14 - 19:20 nested expressions
            |				13:14 - 19:20 expressions block
            |					13:14 - 15:16 alpha numberic word
            |						subId
            |					17:18 - 19:20 alpha numberic word
            |						subId2
            |			21:22 - 23:24 alpha numberic word
            |				id2
            |],
      testCaseParse
        "write a function without args which contains higher priority expressions"
        [ mkRanged 1 2 23 24 $
            Function
              { fSignature = FunctionSignature {fsItems = [FunctionName $ AlphaNumExpr $ mkRanged 1 2 3 4 "Name"], fsURL = ""},
                fBody =
                  FunctionBody $
                    mkRanged
                      25
                      26
                      27
                      28
                      [ ExprsBlock
                          [ AlphaNumExpr $ mkRanged 5 6 7 8 "id",
                            HigherPriorityExpr $
                              mkOcRanged
                                13
                                14
                                15
                                16
                                [ ExprsBlock
                                    [ AlphaNumExpr $ mkRanged 13 14 15 16 "subId",
                                      AlphaNumExpr $ mkRanged 17 18 19 20 "subId2"
                                    ]
                                ],
                            AlphaNumExpr $ mkRanged 21 22 23 24 "id2"
                          ]
                      ]
              }
        ]
        [sbt|
            |1:2 - 23:24 function
            |	1:2 - 3:4 function signature
            |		1:2 - 3:4 name 1
            |			1:2 - 3:4 alpha numberic word
            |				Name
            |	25:26 - 27:28 function body
            |		5:6 - 23:24 expressions block
            |			5:6 - 7:8 alpha numberic word
            |				id
            |			13:14 - 15:16 higher priority expressions
            |				13:14 - 19:20 expressions block
            |					13:14 - 15:16 alpha numberic word
            |						subId
            |					17:18 - 19:20 alpha numberic word
            |						subId2
            |			21:22 - 23:24 alpha numberic word
            |				id2
            |]
    ]
