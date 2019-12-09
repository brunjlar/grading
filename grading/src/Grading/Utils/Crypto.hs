{-# LANGUAGE PartialTypeSignatures #-}

module Grading.Utils.Crypto
    ( salt
    , hash
    ) where

import qualified Crypto.Error         as C
import qualified Crypto.KDF.Argon2    as C
import qualified Crypto.Random        as C
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as LB

import           Grading.Types

salt :: IO ByteString
salt = LB.fromStrict <$> C.getRandomBytes 256

hash :: Password -> Salt -> Hash
hash (Password pw) (Salt s) = case Hash . LB.fromStrict <$> C.hash C.defaultOptions (LB.fromString pw) (LB.toStrict s) 256 of
    C.CryptoPassed h -> h 
    C.CryptoFailed e -> error $ show e
