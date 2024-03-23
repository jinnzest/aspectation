module Pure.Main.Syntax.GeneratorTest
  ( syntaxGeneratorTest,
  )
where

import Control.Monad (Monad (return))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List as L (map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mconcat))
import Data.Text (Text, pack)
import GHC.Integer (Integer, absInteger)
import Main.Syntax.Parsing.Parser (syntaxParser)
import Main.Syntax.Writer.Ranged as R (writeFunctions)
import Main.Syntax.Writer.Source as F (writeFunctions)
import Pure.Main.Syntax.Shared (runParser)
import Shared.Conditional.Debug.Trace (trace)
import Shared.Errors (Errors (MkErrors))
import Shared.Text.Utils (nL, tab)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    arbitraryPrintableChar,
    elements,
    oneof,
    testProperty,
    vectorOf,
  )
import Text.Shakespeare.Text (st)
import Text.Show (Show, show)

genSize :: Gen Int
genSize = elements [1 .. 3]

genNonAlphaNumWord :: Gen Text
genNonAlphaNumWord = do
  size <- genSize
  word <-
    vectorOf size $
      oneof
        [ return '+',
          return '-',
          return '=',
          return '*',
          return '/',
          return '^',
          return '&',
          return ',',
          return '<',
          return '>',
          return '|',
          return '?',
          return '[',
          return ']',
          return '{',
          return '}',
          return '\'',
          return '%',
          return '#',
          return ':',
          return ';',
          return '.',
          return '~',
          return '!',
          return '@',
          return '$',
          return '_',
          return '`',
          return '\\'
        ]
  return $ pack word

splitter :: Text
splitter = [st|#{nL}######################################################################{nL}|]

genTabs :: Gen Text
genTabs = do
  size <- genSize
  tabs <- vectorOf size $ return [st|#{tab}|]
  return $ mconcat tabs

genSingleSpaces :: Gen Text
genSingleSpaces = do
  size <- genSize
  tabs <- vectorOf size $ return " "
  return $ mconcat tabs

genHSpaces :: Gen Text
genHSpaces = oneof [return "", genSingleSpaces, genTabs]

genSpaces :: Gen Text
genSpaces =
  oneof [return [st|#{nL}#{tab}|], genHSpaces]

genAlphaNumWord :: Gen Text
genAlphaNumWord = do
  alphaSize <- genSize
  head <- vectorOf alphaSize $ elements (['A' .. 'Z'] ++ ['a' .. 'z'])
  numericSize <- elements [0 .. 1]
  tail <- vectorOf numericSize $ elements ['0' .. '9']
  return $ pack $ head ++ tail

genSimpleFunctionSignature :: Gen Text
genSimpleFunctionSignature = do
  size <- genSize
  name <- genAlphaNumWord
  items <- vectorOf size $ do
    word <- genAlphaNumWord
    spaces <- genSpaces
    return [st|#{spaces}#{word}|]
  let args = mconcat items
  return [st|#{name}#{args}|]

genOutsideOfBracketsSignatureWord :: Gen Text -> Gen Text
genOutsideOfBracketsSignatureWord generator = oneof [genAlphaNumWord, generator]

genInBracketsSignatureWord :: Gen Text
genInBracketsSignatureWord = do
  word <- genAlphaNumWord
  spaces1 <- genSpaces
  spaces2 <- genSpaces
  return [st|(#{spaces1}#{word}#{spaces2})|]

genSignatureWord :: Gen Text
genSignatureWord = oneof [genOutsideOfBracketsSignatureWord genAlphaNumWord, genInBracketsSignatureWord]

genComplexFunctionSignatureShared :: Gen Text
genComplexFunctionSignatureShared = do
  size <- genSize
  headWord <- genSignatureWord
  tailIds <- vectorOf size $ do
    spaces <- genSpaces
    word <- genSignatureWord
    return [st|#{spaces}#{word}|]
  let tailWordsTxt = mconcat tailIds
  let escapedHashHead = if headWord == "#" then "##" else headWord
  return [st|#{escapedHashHead}#{tailWordsTxt}|]

genFunctionSignature :: Gen Text
genFunctionSignature =
  oneof [genSimpleFunctionSignature, genComplexFunctionSignatureShared]

genExpressionNumber :: Gen Text
genExpressionNumber = do
  int <- arbitrary
  dec <- arbitrary
  let intText = show (int :: Integer)
  let decText = case dec :: Maybe Integer of
        Nothing -> ""
        Just d -> [st|.#{show $ absInteger d}|]
  exp <- arbitrary
  let expText = case exp :: Maybe Integer of
        Nothing -> ""
        Just e -> [st|e#{show e}|]
  spaces <- genSpaces
  return [st|#{spaces}#{intText}#{decText}#{expText}|]

genExpressionText :: Gen Text
genExpressionText = do
  size <- genSize
  spaces <- genSpaces
  textParts <- vectorOf size $ do
    ch <- arbitraryPrintableChar
    return $ if ch == '"' then [st|""|] else [st|#{show ch}|]
  let text = mconcat textParts
  return [st|#{spaces}"#{text}"|]

genExpressionAlphaNum :: Gen Text
genExpressionAlphaNum = do
  spaces <- genSpaces
  word <- genAlphaNumWord
  return [st|#{spaces}#{word}|]

genExpressionNonAlphaNum :: Gen Text
genExpressionNonAlphaNum = do
  spaces <- genSpaces
  word <- genNonAlphaNumWord
  return [st|#{spaces}#{word}|]

genExpressionInBrackets :: Gen Text
genExpressionInBrackets = do
  spaces1 <- genSpaces
  spaces2 <- genSpaces
  expr <- genExpression
  return [st|#{spaces1}(#{expr}#{spaces2})|]

genExpression :: Gen Text
genExpression = do
  spaces <- genSpaces
  expression <-
    oneof
      [genExpressionText, genExpressionNumber, genExpressionAlphaNum, genExpressionNonAlphaNum, genExpressionInBrackets]
  return [st|#{spaces}#{expression}|]

genFunctionBody :: Gen Text
genFunctionBody = do
  size <- genSize
  hSpaces <- genSpaces
  headExpression <- genExpression
  expressionsTail <- vectorOf size $ do
    spaces <- genSpaces
    expr <- genExpression
    return [st|#{spaces}#{expr}|]
  let headExpressionText = [st|#{hSpaces}#{headExpression}|]
  return $ mconcat $ headExpressionText : expressionsTail

genFunction :: Gen Text
genFunction = do
  functionSignature <- genFunctionSignature
  functionBody <- genFunctionBody
  spaces1 <- genSpaces
  spaces2 <- genSpaces
  return [st|#{functionSignature}#{spaces1}->#{functionBody}#{spaces2}|]

type Source :: Type
newtype Source = Source Text deriving stock (Show)

instance Arbitrary Source where
  arbitrary = do
    size <- genSize
    head <- genFunction
    constructs <- vectorOf size genFunction
    return $ Source $ mconcat $ head : map (\c -> [st|#{nL}#{nL}#{c}|]) constructs

syntaxGeneratorTest :: TestTree
syntaxGeneratorTest =
  testGroup
    "Syntax generator tests"
    [ testProperty "" $ \(Source source) ->
        let traceText = [st|Source:#{nL}#{source}|]
            parsedResult = trace traceText $ runParser source syntaxParser
         in case parsedResult of
              Right (tree, trailingSpaces) ->
                let writtenBack = F.writeFunctions tree
                    writtenBackWithSpaces = [st|#{writtenBack}#{trailingSpaces}|]
                    rangedText = R.writeFunctions tree
                 in trace
                      [st|#{nL}source#{splitter}#{source}#{splitter}#{nL}writtenBack#{splitter}#{writtenBackWithSpaces}#{splitter}#{nL}ranged#{nL}#{rangedText}#{nL}#{splitter}|]
                      $ source
                        == [st|#{writtenBackWithSpaces}|]
              Left errors ->
                trace [st|ERRORS: #{errors}|] $
                  errors
                    == MkErrors [] -- trace [st|ERRORS: #{errors}|]
    ]
