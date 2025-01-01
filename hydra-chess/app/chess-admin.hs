module Main where

import Chess (runGame)
import qualified Data.Text as Text
import Game.Server (Host (..))
import Games.Cardano.Network (Network (..))
import Games.Logging (withLogger)
import Games.Server.Hydra (withClient)
import Games.Server.Hydra.Command (interpret)
import System.Environment (getArgs)

main :: IO ()
main = do
  [host, port] <- getArgs
  withLogger ".admin.log" $ \logger -> do
    withClient logger (Host (Text.pack host) (read port)) $ \cnx ->
      getLine >>= interpret logger cnx
