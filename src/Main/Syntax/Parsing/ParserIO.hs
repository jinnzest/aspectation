module Main.Syntax.Parsing.ParserIO
  ( parseSyntaxIO,
  )
where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)
import Main.Syntax.Parsing.Parser (syntaxParser)
import Main.Syntax.Parsing.Tree (SyntaxTree)
import Shared.Errors (Errors)
import Shared.Parser.WrapperIO (parseFile)
import System.IO (IO)

parseSyntaxIO :: Text -> ExceptT Errors IO (SyntaxTree, Text)
parseSyntaxIO filePath = parseFile "Parsing the main language" filePath syntaxParser
