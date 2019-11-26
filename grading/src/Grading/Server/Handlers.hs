{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server.Handlers
    ( gradingServerT
    ) where

import           Control.Exception         (try, SomeException)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as B
import           Database.SQLite.Simple
import           Servant

import           Grading.API
import           Grading.Server.GradingM
import           Grading.Types
import           Grading.Utils.Submit      (submitBS)
import           Grading.Utils.Tar         (checkArchive)

gradingServerT :: ServerT GradingAPI GradingM
gradingServerT = 
         addUserHandler
    :<|> usersHandler
    :<|> tasksHandler
    :<|> uploadHandler

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

tasksHandler :: GradingM [Task]
tasksHandler = withDB $ \conn -> liftIO $ query_ conn "SELECT * FROM tasks ORDER BY id ASC"

uploadHandler :: UserName -> TaskId -> ByteString -> GradingM NoContent
uploadHandler un tid bs = do
    let msg ="upload request from user " ++ show un ++ " for task " ++ show tid ++ ": "
    me <- liftIO $ checkArchive bs
    case me of 
        Nothing  -> do
            res <- liftIO $ submitBS (DockerImage "brunjlar/chains") bs
            logMsg $ msg ++ "OK: " ++ show res
            return NoContent
        Just err -> do
            logMsg $ msg ++ "ERROR: " ++ show err
            throwError $ err400 {errBody = B.fromString $ show err}
