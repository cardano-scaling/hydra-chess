module Main where

import Chess (runGame)
import Games.Cardano.Network (Network (..))
import Games.Run.Hydra (makeGameFlatFile)

main :: IO ()
main = makeGameFlatFile Preprod >>= \f -> putStrLn ("Wrote flat encoding of contract to " <> f)
