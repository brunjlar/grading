{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.Server.Handlers
    ( gradingServerT
    ) where

import Control.Exception         (throwIO, try, Exception (..), SomeException)
import Data.Maybe                (fromJust)
import Data.Time.Clock.POSIX     (getCurrentTime)
import Database.SQLite.Simple
import Servant

import Grading.API
import Grading.Server.GradingM
import Grading.Submission        (Submission (..))
import Grading.Types
import Grading.Utils.CheckResult
import Grading.Utils.Crypto
import Grading.Utils.Submit      (submitArchive)
import Grading.Utils.Tar         (checkArchive_, IsChecked (..))

gradingServerT :: ServerT GradingAPI GradingM
gradingServerT = 
         addUserHandler
    :<|> usersHandler
    :<|> addTaskHandler
    :<|> getTaskHandler
    :<|> getSubmissionHandler
    :<|> postSubmissionHandler

addUserHandler :: UserName -> (EMail, Password) -> GradingM NoContent
addUserHandler n (e, pw) = do
    s <- salt
    let h = hash pw s
        u = User n e s h
    res <- withDB $ \conn -> liftIO $ try $ execute conn "INSERT INTO users (id, email, salt, hash) VALUES (?, ?, ?, ?)" u
    case res of
        Left (err :: SomeException) -> do
            logMsg $ "ERROR adding user " ++ show u ++ ": " ++ show err
            throwError err400
        Right ()                    -> do
            logMsg $ "added user " ++ show u
            return NoContent

usersHandler :: GradingM [User]
usersHandler = withDB $ \conn -> liftIO $ query_ conn "SELECT * FROM users ORDER BY id ASC"

addTaskHandler :: Task Unchecked -> GradingM TaskId
addTaskHandler t = do
    let d = tImage t
    res <- withDB $ \conn -> liftIO $ try $ do
        checkedTask   <- checkArchive_ $ tTask t
        checkedSample <- checkArchive_ $ tSample t
        execute conn "INSERT INTO tasks (image, task, sample) VALUES (?,?,?)" (d, checkedTask, checkedSample)
        [Only tid] <- query_ conn "SELECT last_insert_rowid()"
        resTask    <- submitArchive d checkedTask
        resSample  <- submitArchive d checkedSample
        let taskOK   = testedSatisfying allFail resTask
            sampleOK = testedSatisfying allPass resSample
        if taskOK && sampleOK
            then return tid
            else do
                execute conn "DELETE FROM tasks WHERE id = ?" (Only tid)
                let errTask   = "expected all tests for task to run and fail, but got " ++ show resTask
                    errSample = "expected all tests for sample to run and pass, but got " ++ show resSample
                    err       = if taskOK then errSample
                                          else if sampleOK then errTask
                                                           else errTask ++ ", " ++ errSample
                throwIO $ userError err
    case res of
        Left (err :: SomeException) -> do
            logMsg $ "ERROR adding task with image " ++ show d ++ ": " ++ show err
            throwError err400
        Right tid                   -> do
            logMsg $ "added task " ++ show tid
            return tid

getTaskHandler :: TaskId -> GradingM (Task Checked)
getTaskHandler tid = do
    etask <- withDB $ \conn -> liftIO $ try $ do
        [t] <- query conn "SELECT * FROM tasks where id = ?" (Only tid)
        return t
    case etask of
        Right task                -> do
            logMsg $ "downloaded task " ++ show tid
            return task
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading task " ++ show tid ++ ": " ++ displayException e
            throwError err400

getSubmissionHandler :: SubmissionId -> GradingM (Submission Checked)
getSubmissionHandler sid = do
    esub <- withDB $ \conn -> liftIO $ try $ do
        [sub] <- query conn "SELECT * FROM submissions where id = ?" (Only sid)
        return sub
    case esub of
        Right sub                 -> do
            logMsg $ "downloaded submission " ++ show sid
            return sub
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading submission " ++ show sid ++ ": " ++ displayException e
            throwError err400

postSubmissionHandler :: Submission Unchecked -> GradingM (Submission Checked)
postSubmissionHandler sub = do
    let n   = subUser sub
        tid = subTask sub
    let msg ="upload request from user " ++ show n ++ " for task " ++ show tid ++ ": "
    e <- withDB $ \conn -> liftIO $ try $ do
        checked  <- checkArchive_ $ fromJust $ subArchive sub
        [Only d] <- query conn "SELECT image FROM tasks WHERE id = ?" (Only tid)
        res      <- submitArchive d checked
        now      <- getCurrentTime
        let ma  = case res of
                    Tested _ -> Just checked
                    _        -> Nothing
        execute conn "INSERT INTO submissions (userid, taskid, time, archive, result) VALUES (?,?,?,?,?)" (n, tid, now, ma, res)
        [Only sid] <- query_ conn "SELECT last_insert_rowid()"
        return (sid, now, res)
    case e of
        Left (err :: SomeException) -> do
            logMsg $ msg ++ "ERROR - " ++ displayException err
            throwError err400

        Right (sid, now, res) -> do
            logMsg $ msg ++ "OK - created submission " ++ show sid
            return Submission
                { subId      = sid
                , subUser    = n 
                , subTask    = tid
                , subTime    = Required now
                , subArchive = Nothing
                , subResult  = Required res
                }
