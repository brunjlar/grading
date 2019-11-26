{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grading.Types
    ( UserName (..)
    , EMail (..)
    , User (..)
    , DockerImage (..)
    , TaskId (..)
    , Task (..)
    , ContainerId (..)
    ) where

import Data.Aeson                       (FromJSON, ToJSON)
import Database.SQLite.Simple           (field, FromRow (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField   (ToField (..))
import GHC.Generics                     (Generic)
import Servant

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
    deriving (Show, Read, Eq, Ord)

