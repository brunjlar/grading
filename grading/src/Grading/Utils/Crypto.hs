{-# LANGUAGE PartialTypeSignatures #-}

module Grading.Utils.Crypto
    ( salt
    , hash
    ) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Crypto.Error           as C
import qualified Crypto.KDF.Argon2      as C
import qualified Crypto.Random          as C
import qualified Data.Binary            as B
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.UTF8   as UTF8

import           Grading.Types

salt :: MonadIO m => m Salt
salt = B.decode . B.encode . LB.fromStrict <$> liftIO (C.getRandomBytes 256)

hash :: Password -> Salt -> Hash
hash (Password pw) s = case B.decode . B.encode . LB.fromStrict <$> C.hash C.defaultOptions (UTF8.fromString pw) (LB.toStrict $ B.encode s) 256 of
    C.CryptoPassed h -> h 
    C.CryptoFailed e -> error $ show e
