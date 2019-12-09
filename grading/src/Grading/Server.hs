{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server
    ( Port
    , getPort
    , getAdmins
    , serveGrading
    ) where

import Data.Maybe               (fromMaybe)
import Network.Wai.Handler.Warp (Port, run)
import Servant

import Grading.API
import Grading.Server.GradingM
import Grading.Server.Handlers
import Grading.Types

defaultPort :: Port
defaultPort = 8080

defaultAdmins :: [UserName]
defaultAdmins = [UserName "brunjlar"]

getPort :: Maybe Port -> Port
getPort = fromMaybe defaultPort

getAdmins :: Maybe [UserName] -> [UserName]
getAdmins = fromMaybe defaultAdmins

serveGrading :: Maybe Port -> Maybe [UserName] -> IO ()
serveGrading mport madmins = do
    gc <- initContext $ getAdmins madmins
    run (getPort mport) $ gradingAppT gc

gradingAppT :: GC -> Application
gradingAppT gc = serve gradingAPI $ gradingServer gc

gradingServer :: GC -> Server GradingAPI
gradingServer gc = hoistServer gradingAPI (runGradingM gc) gradingServerT
























