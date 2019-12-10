module Data.Parser.Pad where

import           Data.Pad               (PAD (..))
import qualified Data.Pad               as PAD
import           Data.PadActions
import           Data.Pass              (PASS)
import qualified Data.Pass              as PASS
import           Data.Text
import           Data.Text.Utils

import           Text.Parsec            (anyChar, char, endOfLine, newline,
                                         noneOf, spaces, string)
import           Text.Parsec.Combinator (between, manyTill, optional)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Prim

type ParserT m a = ParsecT Text () m a

betweenDoubleQuotes :: Monad m => ParserT m a -> ParserT m a
betweenDoubleQuotes = between quote quote
  where quote = char '"'

pass :: Monad m => ParserT m PASS
pass = do g <- start
          optional endOfLine
          activities <- next
          return $ PASS.build g activities

start :: Monad m => ParserT m PASS.GUID
start = do spaces *> padTag
           spaces *> guid

next :: Monad m => ParserT m [PASS.Activity]
next = do spaces *> activitiesTag
          optional endOfLine
          many $ spaces *> activity

guid :: Monad m => ParserT m PASS.GUID
guid = betweenDoubleQuotes (many $ noneOf "\"") >>= return . pack

padTag :: Monad m => ParserT m Text
padTag = string "PAD:" >>= return . pack

activitiesTag :: Monad m => ParserT m Text
activitiesTag = string "ACTIVITIES:" >>= return . pack

activity :: Monad m => ParserT m PASS.Activity
activity = do a <- countActivity <|> selectionActivity <|> extractionActivity
              optional endOfLine
              return a

countActivity :: Monad m => ParserT m PASS.Activity
countActivity = do string "COUNT:"
                   g <- spaces *> guid
                   return $ PASS.Activity PASS.Count g

selectionActivity :: Monad m => ParserT m PASS.Activity
selectionActivity = do string "SELECTION:"
                       g <- spaces *> guid
                       return $ PASS.Activity PASS.Selection g

extractionActivity :: Monad m => ParserT m PASS.Activity
extractionActivity =do string "EXTRACTION:"
                       g <- spaces *> guid
                       return $ PASS.Activity PASS.Extraction g

fromPAD :: Monad m => PAD -> m (Either ParseError PASS)
fromPAD (PAD (g, input)) = runParserT pass () (show g) input


