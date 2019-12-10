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
import Control.Monad             (when)
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
import Grading.Utils.Tar         (checkArchive_, IsChecked (..), emptyArchive)

gradingServerT :: ServerT GradingAPI GradingM
gradingServerT = 
         addUserHandler
    :<|> usersHandler
    :<|> addTaskHandler
    :<|> getTaskHandler
    :<|> getSubmissionHandler
    :<|> postSubmissionHandler
    :<|> getSubmissionsHandler

addUserHandler :: UserName -> (EMail, Password) -> GradingM NoContent
addUserHandler n (e, pw) = do
    s <- salt
    b <- isAdmin n
    let h = hash pw s
        r = if b then Admin else Student
        u = User n e r s h
    res <- withDB $ \conn -> liftIO $ try $ execute conn "INSERT INTO users (id, email, role, salt, hash) VALUES (?, ?, ?, ?, ?)" u
    case res of
        Left (err :: SomeException) -> do
            logMsg $ "ERROR adding user " ++ show u ++ ": " ++ show err
            throwError err400
        Right ()                    -> do
            logMsg $ "added user " ++ show u
            return NoContent

usersHandler :: Administrator -> GradingM [User]
usersHandler admin = do
    logMsg $ "authorized administrator " ++ show admin
    withDB $ \conn -> liftIO $ query_ conn "SELECT * FROM users ORDER BY id ASC"

addTaskHandler :: Administrator -> Task Unchecked -> GradingM TaskId
addTaskHandler admin t = do
    logMsg $ "authorized administrator " ++ show admin
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

getTaskHandler :: User -> TaskId -> Bool -> GradingM (Task Checked)
getTaskHandler u tid withSample = do
    when (withSample && userRole u /= Admin) $ throwError err401 -- only admins may see the sample solution!
    logMsg $ "authorized user " ++ show u
    etask <- withDB $ \conn -> liftIO $ try $ do
        [t] <- query conn "SELECT * FROM tasks where id = ?" (Only tid)
        return t
    case etask of
        Right task                -> do
            logMsg $ "downloaded task " ++ show tid
            return $ if withSample then task else task {tSample = emptyArchive}
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading task " ++ show tid ++ ": " ++ displayException e
            throwError err400

getSubmissionHandler :: Administrator -> SubmissionId -> GradingM (Submission Checked)
getSubmissionHandler admin sid = do
    logMsg $ "authorized administrator " ++ show admin
    esub <- withDB $ \conn -> liftIO $ try $ do
        [sub] <- query conn "SELECT * FROM submissions WHERE id = ?" (Only sid)
        return sub
    case esub of
        Right sub                 -> do
            logMsg $ "downloaded submission " ++ show sid
            return sub
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading submission " ++ show sid ++ ": " ++ displayException e
            throwError err400

postSubmissionHandler :: User -> Submission Unchecked -> GradingM (Submission Checked)
postSubmissionHandler u sub = do
    logMsg $ "authorized user " ++ show u
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
        execute conn "INSERT INTO submissions (userid, taskid, time, archive, result, remark) VALUES (?,?,?,?,?,?)" (n, tid, now, ma, res, Nothing :: Maybe String)
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
                , subRemark  = Nothing
                }

getSubmissionsHandler :: Administrator -> UserName -> TaskId -> GradingM [Submission Checked]
getSubmissionsHandler admin n tid = do
    logMsg $ "authorized administrator " ++ show admin
    esubs <- withDB $ \conn -> liftIO $ try $
        query conn "SELECT * FROM submissions WHERE userid = ? AND taskid = ?" (n, tid)
    case esubs of
        Right subs                -> do
            logMsg $ "downloaded " ++ show (length subs) ++ " submission(s) for user " ++ show n ++ " and task " ++ show tid
            return $ map (\sub -> sub{subArchive = Nothing}) subs
        Left (e :: SomeException) -> do
            logMsg $ "ERROR downloading submissions for user " ++ show n ++ " and task " ++ show tid ++ ": " ++ displayException e
            throwError err400
