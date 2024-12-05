{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Chess.ParseSpec where

import Chess.Game (Move (..))
import Chess.Parse (parseMove)
import Chess.Render (Render (..))
import Data.Function ((&))
import Data.Text (unpack)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, label, (===))

spec :: Spec
spec = do
  prop "parses and render moves" parse_and_render_moves

parse_and_render_moves :: Move -> Property
parse_and_render_moves move =
  let repr = unpack $ render move
   in parseMove repr
        === Right move
        & counterexample ("rendered: " <> repr)
        & label (typeOf move)
 where
  typeOf :: Move -> String
  typeOf = head . words . show
