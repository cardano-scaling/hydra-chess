{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Games.Server.Hydra.Command where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode', encode, object, (.=))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Games.Logging (Logger, logWith)
import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import Text.Megaparsec (Parsec, empty, manyTill, parse, try, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Command = Exit | NewTx FilePath

data CommandLog
  = ErrorReadingTransaction String
  | NewTxSubmitted FilePath
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

interpret :: Logger -> Connection -> String -> IO ()
interpret logger cnx cmd =
  case parseCommand cmd of
    Left err -> putStrLn $ "Cannot parse command " <> cmd <> ", error: " <> err
    Right Exit -> pure ()
    Right (NewTx file) -> do
      envelope <- eitherDecode' @Value <$> LBS.readFile file
      case envelope of
        Left err -> do
          logWith logger $ ErrorReadingTransaction err
          pure ()
        Right tx -> do
          WS.sendTextData
            cnx
            ( encode $
                object
                  [ "tag" .= ("NewTx" :: Text)
                  , "transaction" .= tx
                  ]
            )

          logWith logger $ NewTxSubmitted file

parseCommand :: String -> Either String Command
parseCommand =
  first show . parse commandParser "" . pack

commandParser :: Parser Command
commandParser =
  exitParser
    <|> newTxParser

exitParser :: Parser Command
exitParser = (try (string "x") <|> string "exit") $> Exit

newTxParser :: Parser Command
newTxParser = do
  string "newTx" >> spaceConsumer
  NewTx <$> (char '\"' *> manyTill charLiteral (char '\"'))

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty
