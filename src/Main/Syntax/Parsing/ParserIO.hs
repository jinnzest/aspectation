module Main.Syntax.Parsing.ParserIO
  ( parseSyntaxIO,
  )
where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)
import Main.Syntax.Parsing.Parser (syntaxParsing)
import Main.Syntax.Parsing.Tree (SyntaxTree)
import Shared.Errors (Errors)
import Shared.Parser.WrapperIO (parseFile)
import System.IO (IO)

parseSyntaxIO :: Text -> Text -> ExceptT Errors IO (SyntaxTree, Text)
parseSyntaxIO filePath fileURL =
  parseFile "Parsing the main language" filePath (syntaxParsing fileURL)
