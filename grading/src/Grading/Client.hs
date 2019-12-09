{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.Client
    ( User
    , getPort
    , addUserIO
    , usersIO
    , addTaskIO
    , getTaskIO
    , getSubmissionIO
    , postSubmissionIO
    ) where

import Codec.Archive.Tar      (pack, write)
import Codec.Compression.GZip (compress)
import Control.Exception      (throwIO)
import Control.Monad          (void)
import Network.HTTP.Client    (newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutNone)
import Servant
import Servant.Client
import System.IO.Error        (userError)

import Grading.API
import Grading.Server         (getPort)
import Grading.Submission
import Grading.Types
import Grading.Utils.Tar      (archive, IsChecked (..), normFolder)

addUser        :: UserName -> (EMail, Password) -> ClientM NoContent
users          :: ClientM [User]
addTask        :: Task Unchecked -> ClientM TaskId
getTask        :: TaskId -> ClientM (Task Checked) 
getSubmission  :: SubmissionId -> ClientM (Submission Checked)
postSubmission :: Submission Unchecked -> ClientM (Submission Checked)
addUser :<|> users :<|> addTask :<|> getTask :<|> getSubmission :<|> postSubmission = client gradingAPI

clientIO :: String -> Int -> ClientM a -> IO a
clientIO host port c = do
    m <- newManager $ defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
    let env = mkClientEnv m $ BaseUrl Http host port ""
    res <- runClientM c env
    case res of
        Left err -> throwIO $ userError $ show err
        Right a  -> return a

addUserIO :: String -> Int -> UserName -> EMail -> Password -> IO ()
addUserIO host port n e pw = void $ clientIO host port $ addUser n (e, pw) 

usersIO :: String -> Int -> IO [User]
usersIO host port = clientIO host port users

addTaskIO :: String -> Int -> Task Unchecked -> IO TaskId
addTaskIO host port td = clientIO host port $ addTask td

getTaskIO :: String -> Int -> TaskId -> IO (Task Checked) 
getTaskIO host port tid = clientIO host port $ getTask tid

getSubmissionIO :: String -> Int -> SubmissionId -> IO (Submission Checked)
getSubmissionIO host port sid = clientIO host port $ getSubmission sid

postSubmissionIO :: String -> Int -> UserName -> TaskId -> FilePath -> IO (Submission Checked)
postSubmissionIO host port n tid fp = do
    nfp       <- normFolder fp
    unchecked <- archive . compress . write <$> pack nfp ["."]
    let sub = Submission
                { subId      = NotRequired
                , subUser    = n
                , subTask    = tid
                , subTime    = NotRequired
                , subArchive = Just unchecked
                , subResult  = NotRequired
                }
    clientIO host port $ postSubmission sub
