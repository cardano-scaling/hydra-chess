{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BlackJack.Client.Console where

import BlackJack.Client.IO (Command (NewTable, Quit), Err (..), HasIO (..))
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, many, parse, sepBy, try)
import Text.Megaparsec.Char (alphaNumChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

instance HasIO IO where
  input = do
    inp <- readInput <$> Text.getLine
    case inp of
      Left err -> pure $ Left $ Err err
      Right cmd -> pure $ Right cmd
  output = print
  prompt = putStr "> "

readInput :: Text -> Either Text Command
readInput = first (pack . show) . parse inputParser ""

inputParser :: Parser Command
inputParser = quitParser <|> newTableParser

quitParser :: Parser Command
quitParser = (try (string "q") <|> string "quit") $> Quit

newTableParser :: Parser Command
newTableParser = do
  string "newTable" >> spaceConsumer
  NewTable <$> sepBy identifier space

identifier :: Parser Text
identifier = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty