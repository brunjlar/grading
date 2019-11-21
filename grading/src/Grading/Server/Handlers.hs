{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server.Handlers
    ( gradingServerT
    ) where

import           Control.Monad.Except (MonadError (..))
import           Data.ByteString.Lazy (ByteString)
import           Servant
import           System.FilePath ((</>))
import           Text.Encoding.Z (zEncodeString)

import           Grading.API
import           Grading.Server.Monad.Class

gradingServerT :: MonadGrading m => ServerT GradingAPI m
gradingServerT = 
         usersHandler
    :<|> tasksHandler
    :<|> uploadHandler

usersHandler :: MonadGrading m => m [User]
usersHandler = undefined

tasksHandler :: MonadGrading m => m [Task]
tasksHandler = undefined

uploadHandler :: MonadGrading m => User -> Task -> ByteString -> m NoContent
uploadHandler user task bs = do
    let msg ="upload request from user " ++ show user ++ " for task " ++ show task ++ ": "
        f = getUploadFolder user task
    catchError (extractFolder f bs >> logMsg (msg ++ "extracted to folder " ++ show f)) $ \e -> do
        logMsg $ msg ++ "ERROR: " ++ show e
        throwError e
    return NoContent

getUploadFolder :: User -> Task -> FilePath
getUploadFolder user task = zEncodeString user </> show task
