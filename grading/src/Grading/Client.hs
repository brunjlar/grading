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
import Grading.Utils.Auth
import Grading.Utils.Tar      (archive, IsChecked (..), normFolder)

addUser        :: UserName -> (EMail, Password) -> ClientM NoContent
users          :: BasicAuthData -> ClientM [User]
addTask        :: BasicAuthData -> Task Unchecked -> ClientM TaskId
getTask        :: BasicAuthData -> TaskId -> Bool -> ClientM (Task Checked) 
getSubmission  :: BasicAuthData -> SubmissionId -> ClientM (Submission Checked)
postSubmission :: BasicAuthData -> Submission Unchecked -> ClientM (Submission Checked)
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

usersIO :: String -> Int -> UserName -> Password -> IO [User]
usersIO host port n pw = clientIO host port $ users $ toAuthData n pw

addTaskIO :: String -> Int -> UserName -> Password -> Task Unchecked -> IO TaskId
addTaskIO host port n pw td = clientIO host port $ addTask (toAuthData n pw) td

getTaskIO :: String -> Int -> UserName -> Password -> TaskId -> Bool -> IO (Task Checked) 
getTaskIO host port n pw tid withSample = clientIO host port $ getTask (toAuthData n pw) tid withSample

getSubmissionIO :: String -> Int -> UserName -> Password -> SubmissionId -> IO (Submission Checked)
getSubmissionIO host port n pw sid = clientIO host port $ getSubmission (toAuthData n pw) sid

postSubmissionIO :: String -> Int -> UserName -> Password -> TaskId -> FilePath -> IO (Submission Checked)
postSubmissionIO host port n pw tid fp = do
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
    clientIO host port $ postSubmission (toAuthData n pw) sub
