{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server.Handlers
    ( gradingServerT
    ) where

import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as B
import           Servant

import           Grading.API
import           Grading.Server.GradingM
import           Grading.Utils.Submit      (submitBS)
import           Grading.Utils.Tar         (checkArchive)

gradingServerT :: ServerT GradingAPI GradingM
gradingServerT = 
         usersHandler
    :<|> tasksHandler
    :<|> uploadHandler

usersHandler :: GradingM [User]
usersHandler = undefined

tasksHandler :: GradingM [Task]
tasksHandler = undefined

uploadHandler :: User -> Task -> ByteString -> GradingM NoContent
uploadHandler user task bs = do
    let msg ="upload request from user " ++ show user ++ " for task " ++ show task ++ ": "
    me <- liftIO $ checkArchive bs
    case me of 
        Nothing  -> do
            res <- liftIO $ submitBS "brunjlar/chains" bs
            logMsg $ msg ++ "OK: " ++ show res
            return NoContent
        Just err -> do
            logMsg $ msg ++ "ERROR: " ++ show err
            throwError $ err400 {errBody = B.fromString $ show err}
