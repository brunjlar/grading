{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grading.Submission
    ( Submission (..)
    ) where

import Data.Binary            (Binary)
import Data.Time              (UTCTime)
import Database.SQLite.Simple (FromRow (..))
import GHC.Generics           (Generic)
import Servant

import Grading.Types
import Grading.Utils.Tar      (CheckedArchive)

data Submission = Submission
    { subId      :: !SubmissionId
    , subUser    :: !UserName
    , subTask    :: !TaskId
    , subTime    :: !UTCTime
    , subArchive :: !(Maybe CheckedArchive)
    , subResult  :: !Result
    } deriving (Show, Read, Eq, Ord, Generic, Binary)

instance FromRow Submission where
    fromRow = (\(sid, n, tid, t, ma, r) -> Submission sid n tid t ma r) <$> fromRow

instance MimeRender OctetStream Submission where
    mimeRender = mimeRenderBinary

instance MimeUnrender OctetStream Submission where
    mimeUnrender = mimeUnrenderBinary
