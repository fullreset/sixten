{-# LANGUAGE OverloadedStrings #-}
module Error where

import Data.Monoid
import Data.Text(Text)
import Data.Text.Prettyprint.Doc(annotate, line)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Parsix.Highlight
import Text.Parsix.Position

import Pretty
import Util

data SourceLocation = SourceLocation
  { sourceLocFile :: FilePath
  , sourceLocSpan :: !Span
  , sourceLocSource :: Text
  , sourceLocHighlights :: Highlights
  } deriving (Eq, Ord, Show)

locationSummary :: SourceLocation -> Doc
locationSummary src
  = shower (sourceLocFile src)
  <> ":" <> shower (visualRow loc)
  <> ":" <> shower (visualColumn loc)
  where
    loc = spanStart $ sourceLocSpan src

-- TODO handle spans and not just the start position
locationRendering :: SourceLocation -> Doc
locationRendering src = prettyPosition
  defaultStyle
  (spanStart $ sourceLocSpan src)
  (sourceLocSource src)
  (sourceLocHighlights src)

data ErrorKind
  = SyntaxError
  | TypeError
  | CommandLineError
  deriving Show

instance Pretty ErrorKind where
  pretty SyntaxError = "Syntax error"
  pretty TypeError = "Type error"
  pretty CommandLineError = "Command-line error"

data Error = Error
  { errorKind :: !ErrorKind
  , errorSummary :: !Doc
  , errorLocation :: !(Maybe SourceLocation)
  , errorFootnote :: !Doc
  } deriving Show

syntaxError, typeError :: Doc -> SourceLocation -> Doc -> Error
syntaxError h loc = Error SyntaxError h (Just loc)
typeError h loc = Error TypeError h (Just loc)

commandLineError :: Doc -> Doc -> Error
commandLineError h = Error CommandLineError h Nothing

printError :: Error -> IO ()
printError (Error kind summary (Just loc) footnote) = putDoc
  $ locationSummary loc <> ":" <> annotate (color Red) (pretty kind) <> ":" <> summary
  <> line <> locationRendering loc
  <> line <> footnote
printError (Error kind summary Nothing footnote) = putDoc
  $ annotate (color Red) (pretty kind) <> ":" <> summary
  <> line <> footnote
