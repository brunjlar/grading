{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans                        #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.Types
    ( module Grading.Utils.Result
    , UserName (..)
    , EMail (..)
    , User (..)
    , DockerImage (..)
    , Require (..)
    , required
    , TaskId (..)
    , Task (..)
    , ContainerId (..)
    , SubmissionId (..)
    , Password (..)
    , Salt (..)
    , Hash (..)
    , mimeRenderBinary
    , mimeUnrenderBinary
    ) where

import           Control.Exception                (ErrorCall (..), SomeException (..))
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Binary                      (Binary (..), decodeOrFail, encode)
import qualified Data.Binary                      as B
import           Data.ByteString.Lazy             (ByteString)
import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (..))
import           Data.Time                        (UTCTime)
import           Data.Typeable                    (Typeable, typeRep)
import           Database.SQLite.Simple           (field, FromRow (..))
import           Database.SQLite.Simple.FromField (Field, FromField (..))
import           Database.SQLite.Simple.Ok        (Ok (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           Database.SQLite3                 (SQLData)
import           GHC.Generics                     (Generic)
import           Servant
import           Text.Read                        (readMaybe)

import           Grading.Utils.Result
import           Grading.Utils.Tar

newtype UserName = UserName String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Binary, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype EMail = EMail String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Binary, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

data User = User 
    { userName  :: !UserName
    , userEMail :: !EMail
    } deriving (Show, Read, Eq, Ord, Generic, Binary, FromJSON, ToJSON)

instance FromRow User where
    fromRow = User <$> field <*> field

newtype DockerImage = DockerImage String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Binary, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype TaskId = TaskId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Binary, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

data Require (c :: IsChecked) (a :: Type) :: Type where
    NotRequired :: Require Unchecked a
    Required    :: a -> Require Checked a

required :: Require Checked a -> a
required (Required a) = a

deriving instance Show a => Show (Require c a)
deriving instance Eq a => Eq (Require c a)
deriving instance Ord a => Ord (Require c a)

instance Read (Require Unchecked a) where
    readsPrec _ s = case take 11 s of
        "NotRequired" -> [(NotRequired, drop 11 s)]
        _             -> []

instance Read a => Read (Require Checked a) where
    readsPrec d = readParen (d > 10) $ \s ->
        [(Required a, u) | ("Required", t) <- lex s,
                           (a, u) <- readsPrec 11 t]

instance Binary a => Binary (Require Checked a) where
    put (Required a) = put a
    get = Required <$> get

instance Binary a => Binary (Require Unchecked a) where
    put NotRequired = return ()
    get = return NotRequired

instance FromField a => FromField (Require Checked a) where
    fromField = fmap Required . fromField

data Task (c :: IsChecked) = Task
    { tId     :: !(Require c TaskId)
    , tImage  :: !DockerImage
    , tTask   :: !(Archive c)
    , tSample :: !(Archive c)
    } deriving stock (Show, Eq, Ord, Generic)

deriving instance Read (Task Unchecked)
deriving instance Read (Task Checked)
deriving instance Binary (Task Unchecked)
deriving instance Binary (Task Checked)

instance FromRow (Task Checked) where
    fromRow = Task <$> field <*> field <*> field <*> field

instance MimeRender OctetStream (Task Unchecked) where
    mimeRender = mimeRenderBinary

instance MimeRender OctetStream (Task Checked) where
    mimeRender = mimeRenderBinary

instance MimeUnrender OctetStream (Task Unchecked) where
    mimeUnrender = mimeUnrenderBinary

instance MimeUnrender OctetStream (Task Checked) where
    mimeUnrender = mimeUnrenderBinary

newtype ContainerId = ContainerId String
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromField, ToField)

newtype SubmissionId = SubmissionId Int
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (Binary, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

instance FromJSON TestResult
instance FromJSON TestsAndHints
instance FromJSON Result
instance ToJSON TestResult
instance ToJSON TestsAndHints
instance ToJSON Result

instance Binary UTCTime where
    put = putShow
    get = getRead

instance Binary TestResult where

instance Binary TestsAndHints where

instance FromField TestsAndHints where
    fromField = fromFieldRead

instance ToField TestsAndHints where
    toField = toFieldShow

instance Binary Result where

instance FromField Result where
    fromField = fromFieldRead

instance ToField Result where
    toField = toFieldShow

newtype Password = Password String
    deriving newtype (Show, Read, Eq, Ord)

newtype Salt = Salt ByteString
    deriving newtype (Show, Read, Eq, Ord, Binary, FromField, ToField)

newtype Hash = Hash ByteString
    deriving newtype (Show, Read, Eq, Ord, Binary, FromField, ToField)

fromFieldRead :: forall a. (Read a, Typeable a) => Field -> Ok a
fromFieldRead f = case fromField f of
    Ok s      -> case readMaybe s of
        Nothing -> Errors [SomeException $ ErrorCall $ "can't parse '" ++ s ++ "' as " ++ show (typeRep (Proxy :: Proxy a))]
        Just r  -> Ok r
    Errors es -> Errors es

toFieldShow :: Show a => a -> SQLData
toFieldShow = toField . show

putShow :: Show a => a -> B.Put
putShow = put . show

getRead :: Read a => B.Get a
getRead = do
    s <- get
    case readMaybe s of
        Just a  -> return a
        Nothing -> fail $ "getRead: can't parse '" ++ s ++ "'"

mimeRenderBinary :: Binary a => Proxy OctetStream -> a -> ByteString
mimeRenderBinary p = mimeRender p . encode

mimeUnrenderBinary :: Binary a => Proxy OctetStream -> ByteString -> Either String a
mimeUnrenderBinary p bs = do
    cs <- mimeUnrender p bs
    case decodeOrFail cs of
        Left (_, _, e)   -> Left e
        Right (_, _, td) -> Right td
