{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Games.Cardano.Crypto where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Curve25519 (PublicKey, SecretKey, publicKey, secretKey)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

instance ToCBOR SecretKey where
  toCBOR = toCBOR . convert @_ @ByteString

instance FromCBOR SecretKey where
  fromCBOR = do
    bs <- fromCBOR @ByteString
    case secretKey bs of
      CryptoPassed sk -> pure sk
      CryptoFailed err -> fail $ show err

instance ToCBOR PublicKey where
  toCBOR = toCBOR . convert @_ @ByteString

instance FromCBOR PublicKey where
  fromCBOR = do
    bs <- fromCBOR @ByteString
    case publicKey bs of
      CryptoPassed vk -> pure vk
      CryptoFailed err -> fail $ show err
