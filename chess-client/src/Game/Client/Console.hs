{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Client.Console (
  mkImpureIO,
  inputParser,
  helpText,
  Coins (..),
  Coin (..),
  toListOfTokens,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, handle, throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Game.Client.IO (Command (..), Err (..), HasIO (..))
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (isEOFError)
import Text.Megaparsec (Parsec, empty, many, parse, sepBy, takeRest, try)
import Text.Megaparsec.Char (alphaNumChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

helpText :: Text
helpText =
  Text.unlines
    [ "Commands:"
    , " * help : Display this message"
    , " * init : Starts a new game session opening a Hydra head (can take a while)"
    , " * newGame : Starts a new game when head is opened or a game concludes with"
    , "       a check-mate. Displays the initial board."
    , " * play <from>-<to> : move a piece on the board <from> some location <to>"
    , "       some other location. <from> and <to> are denoted using cartesian"
    , "       coordinates, for example:"
    , "       > play d2-d4"
    , "       Displays updated board upon validation of the move"
    , " * stop : Stops the current game session, closing the Hydra head (can take a while)"
    , " * quit : Quits hydra-chess gracefully (Ctrl-D or Ctrl-C also works)"
    ]

mkImpureIO :: (Show output) => Parser command -> HasIO command output IO
mkImpureIO parser =
  HasIO
    { input
    , output = print
    , problem = print
    , exit = throwIO ExitSuccess
    , prompt = putStr "> "
    }
 where
  input = handle eofException $ do
    line <- Text.getLine
    if line == ""
      then input
      else case readInput parser line of
        Left err -> pure $ Left $ Err err
        Right cmd -> pure $ Right cmd

eofException :: IOException -> IO (Either Err command)
eofException e
  | isEOFError e = pure (Left EOF)
  | otherwise = pure (Left $ Err $ pack $ show e)

readInput :: Parser command -> Text -> Either Text command
readInput parser = first (pack . show) . parse parser ""

inputParser :: Parser Command
inputParser =
  quitParser
    <|> newTableParser
    <|> playParser
    <|> newGameParser
    <|> stopParser

quitParser :: Parser Command
quitParser = (try (string "q") <|> string "quit") $> Quit

newTableParser :: Parser Command
newTableParser = do
  string "newTable" >> spaceConsumer
  NewTable <$> sepBy identifier space

playParser :: Parser Command
playParser = do
  string "play" >> spaceConsumer
  Play <$> parsePlay
 where
  parsePlay :: Parser Text
  parsePlay = takeRest

newGameParser :: Parser Command
newGameParser = do
  string "newGame" >> spaceConsumer
  pure NewGame

stopParser :: Parser Command
stopParser = do
  string "stop" >> spaceConsumer
  pure Stop

identifier :: Parser Text
identifier = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty

data SimpleUTxO
  = SimpleUTxO {txIn :: Text, coins :: Coins}
  | UTxOWithDatum {txIn :: Text, coins :: Coins, datum :: Datum}
  deriving stock (Eq, Show)

data Datum = DatumHash Text | DatumInt Integer
  deriving stock (Eq, Show)

data Coins = Coins
  { lovelace :: Integer
  , natives :: Map Text Coin
  }
  deriving stock (Eq, Show)

newtype Coin = Coin (Map Text Integer)
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

instance FromJSON Coin where
  parseJSON = fmap Coin . parseJSON

instance ToJSON Coins where
  toJSON Coins{lovelace, natives} =
    object $
      ["lovelace" .= lovelace]
        <> fmap
          ( \(pid, coin) ->
              fromText pid .= coin
          )
          (Map.toList natives)

instance FromJSON Coins where
  parseJSON = withObject "Coins" $ \kv ->
    Coins
      <$> kv .: "lovelace"
      <*> (Map.fromList <$> traverse parsePair (filter ((/= "lovelace") . fst) $ KeyMap.toList kv))
   where
    parsePair :: (Aeson.Key, Aeson.Value) -> Aeson.Parser (Text, Coin)
    parsePair (k, v) = (Key.toText k,) <$> parseJSON v

toListOfTokens :: Map Text Coin -> [Text]
toListOfTokens = Map.foldrWithKey (\k c acc -> toListOfCoins k c <> acc) []

toListOfCoins :: Text -> Coin -> [Text]
toListOfCoins currency (Coin coins) =
  Map.foldrWithKey (\k v acc -> pack (show v) <> " " <> currency <> "." <> k : acc) [] coins
