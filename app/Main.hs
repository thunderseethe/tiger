module Main where

import qualified Data.Text.IO as TIO

import Parser
import Text.Megaparsec.Error
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import AbstractSyntaxTree

main :: IO ()
main = do
  stdin <- TIO.getContents
  case parseProgram stdin of
    Left err -> putStrLn (parseErrorPretty err)
    Right prog -> putDoc (pretty prog)
