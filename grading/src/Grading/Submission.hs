{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.Submission
    ( Submission (..)
    ) where

import Data.Binary            (Binary)
import Data.Time              (UTCTime)
import Database.SQLite.Simple (FromRow (..))
import GHC.Generics           (Generic)
import Servant

import Grading.Types
import Grading.Utils.Tar      (Archive, IsChecked (..))

data Submission (c :: IsChecked) = Submission
    { subId      :: !(Require c SubmissionId)
    , subUser    :: !UserName
    , subTask    :: !TaskId
    , subTime    :: !(Require c UTCTime)
    , subArchive :: !(Maybe (Archive c))
    , subResult  :: !(Require c Result)
    , subRemark  :: !(Maybe String)
    } deriving (Show, Eq, Ord, Generic)

deriving instance Read (Submission Unchecked)
deriving instance Read (Submission Checked)
deriving instance Binary (Submission Unchecked)
deriving instance Binary (Submission Checked)

instance FromRow (Submission Checked) where
    fromRow = (\(sid, n, tid, t, ma, r, rm) -> Submission sid n tid t ma r rm) <$> fromRow

instance MimeRender OctetStream (Submission Unchecked) where
    mimeRender = mimeRenderBinary

instance MimeRender OctetStream (Submission Checked) where
    mimeRender = mimeRenderBinary

instance MimeUnrender OctetStream (Submission Unchecked) where
    mimeUnrender = mimeUnrenderBinary

instance MimeUnrender OctetStream (Submission Checked) where
    mimeUnrender = mimeUnrenderBinary

instance MimeRender OctetStream [Submission Checked] where
    mimeRender = mimeRenderBinary

instance MimeUnrender OctetStream [Submission Checked] where
    mimeUnrender = mimeUnrenderBinary

