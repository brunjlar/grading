{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server.Handlers
    ( gradingServerT
    ) where

import Control.Exception         (throwIO, try, Exception (..), SomeException)
import Data.Time.Clock.POSIX     (getCurrentTime)
import Database.SQLite.Simple
import Servant

import Grading.API
import Grading.Server.GradingM
import Grading.Submission        (Submission (..))
import Grading.Types
import Grading.Utils.CheckResult
import Grading.Utils.Submit      (submitArchive)
import Grading.Utils.Tar         (CheckedArchive, checkArchive_)

gradingServerT :: ServerT GradingAPI GradingM
gradingServerT = 
         addUserHandler
    :<|> usersHandler
    :<|> addTaskHandler
    :<|> getTaskHandler
    :<|> getSubmissionHandler
    :<|> postSubmissionHandler

addUserHandler :: UserName -> EMail -> GradingM NoContent
addUserHandler n e = do
    let u = User n e
    res <- withDB $ \conn -> liftIO $ try $ execute conn "INSERT INTO users (id, email) VALUES (?,?)" (n, e)
    case res of
        Left (err :: SomeException) -> do
            logMsg $ "ERROR adding user " ++ show u ++ ": " ++ show err
            throwError err400
        Right ()                    -> do
            logMsg $ "added user " ++ show u
            return NoContent

usersHandler :: GradingM [User]
usersHandler = withDB $ \conn -> liftIO $ query_ conn "SELECT * FROM users ORDER BY id ASC"

addTaskHandler :: TaskDescription -> GradingM TaskId
addTaskHandler td = do
    let d = tdImage td
    res <- withDB $ \conn -> liftIO $ try $ do
        checked <- checkArchive_ $ tdArchive td
        execute conn "INSERT INTO tasks (image, archive) VALUES (?,?)" (d, checked)
        [Only tid] <- query_ conn "SELECT last_insert_rowid()"
        res        <- submitArchive d checked
        if testedSatisfying allFail res
            then return tid
            else do 
                execute conn "DELETE FROM tasks WHERE id = ?" (Only tid)
                throwIO $ userError $ "expected all tests to run and fail, but got: " ++ show res
    case res of
        Left (err :: SomeException) -> do
            logMsg $ "ERROR adding task with image " ++ show d ++ ": " ++ show err
            throwError err400
        Right tid                   -> do
            logMsg $ "added task " ++ show tid
            return tid

getTaskHandler :: TaskId -> GradingM CheckedArchive
getTaskHandler tid = do
    echecked <- withDB $ \conn -> liftIO $ try $ do
        [Only a] <- query conn "SELECT archive FROM tasks where id = ?" (Only tid)
        return a
    case echecked of
        Right checked             -> do
            logMsg $ "downloaded task " ++ show tid
            return checked
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading task " ++ show tid ++ ": " ++ displayException e
            throwError err400

getSubmissionHandler :: SubmissionId -> GradingM Submission
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

postSubmissionHandler :: UserName -> TaskId -> UncheckedArchive -> GradingM Submission
postSubmissionHandler n tid unchecked = do
    let msg ="upload request from user " ++ show n ++ " for task " ++ show tid ++ ": "
    e <- withDB $ \conn -> liftIO $ try $ do
        checked  <- checkArchive_ unchecked
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
                , subTime    = now
                , subArchive = Nothing
                , subResult  = res
                }
