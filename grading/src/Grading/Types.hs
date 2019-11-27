{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Grading.Types
    ( module Grading.Utils.Result
    , UserName (..)
    , EMail (..)
    , User (..)
    , DockerImage (..)
    , TaskId (..)
    , TaskDescription (..)
    , ContainerId (..)
    , SubmissionId (..)
    , UncheckedArchive (..)
    ) where

import Control.Exception                (ErrorCall (..), SomeException (..))
import Data.Aeson                       (FromJSON, ToJSON)
import Data.Binary                      (Binary (..), decodeOrFail, encode)
import Data.ByteString.Lazy             (ByteString)
import Data.Proxy                       (Proxy (..))
import Data.Typeable                    (Typeable, typeRep)
import Database.SQLite.Simple           (field, FromRow (..))
import Database.SQLite.Simple.FromField (Field, FromField (..))
import Database.SQLite.Simple.Ok        (Ok (..))
import Database.SQLite.Simple.ToField   (ToField (..))
import Database.SQLite3                 (SQLData)
import GHC.Generics                     (Generic)
import Servant
import Text.Read                        (readMaybe)

import Grading.Utils.Result

newtype UserName = UserName String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype EMail = EMail String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

data User = User 
    { userName  :: UserName
    , userEMail :: EMail
    } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

instance FromRow User where
    fromRow = User <$> field <*> field

newtype DockerImage = DockerImage String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField, Binary)

newtype TaskId = TaskId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

data TaskDescription = TaskDescription
    { tdImage   :: DockerImage
    , tdArchive :: UncheckedArchive
    } deriving stock (Show, Read, Eq, Ord, Generic)
      deriving anyclass (Binary)

instance MimeRender OctetStream TaskDescription where
    mimeRender = mimeRenderBinary

instance MimeUnrender OctetStream TaskDescription where
    mimeUnrender = mimeUnrenderBinary

newtype ContainerId = ContainerId String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

newtype SubmissionId = SubmissionId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

newtype UncheckedArchive = UncheckedArchive ByteString
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream, FromField, ToField, Binary)

instance FromJSON TestResult
instance FromJSON TestsAndHints
instance ToJSON TestResult
instance ToJSON TestsAndHints

instance FromField TestsAndHints where
    fromField = fromFieldRead

instance ToField TestsAndHints where
    toField = toFieldShow

instance FromField Result where
    fromField = fromFieldRead

instance ToField Result where
    toField = toFieldShow

fromFieldRead :: forall a. (Read a, Typeable a) => Field -> Ok a
fromFieldRead f = case fromField f of
    Ok s      -> case readMaybe s of
        Nothing -> Errors [SomeException $ ErrorCall $ "can't parse '" ++ s ++ "' as " ++ show (typeRep (Proxy :: Proxy a))]
        Just r  -> Ok r
    Errors es -> Errors es

toFieldShow :: Show a => a -> SQLData
toFieldShow = toField . show

mimeRenderBinary :: Binary a => Proxy OctetStream -> a -> ByteString
mimeRenderBinary p = mimeRender p . encode

mimeUnrenderBinary :: Binary a => Proxy OctetStream -> ByteString -> Either String a
mimeUnrenderBinary p bs = do
    cs <- mimeUnrender p bs
    case decodeOrFail cs of
        Left (_, _, e)   -> Left e
        Right (_, _, td) -> Right td
