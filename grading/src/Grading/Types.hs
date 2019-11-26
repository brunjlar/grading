{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Grading.Types
    ( module Grading.Utils.Result
    , UserName (..)
    , EMail (..)
    , User (..)
    , DockerImage (..)
    , TaskId (..)
    , Task (..)
    , ContainerId (..)
    , SubmissionId (..)
    ) where

import Control.Exception                (ErrorCall (..), SomeException (..))
import Data.Proxy                       (Proxy (..))
import Data.Aeson                       (FromJSON, ToJSON)
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
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype TaskId = TaskId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

data Task = Task
    { taskId    :: TaskId
    , taskImage :: DockerImage
    } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

instance FromRow Task where
    fromRow = Task <$> field <*> field

newtype ContainerId = ContainerId String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

newtype SubmissionId = SubmissionId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

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
