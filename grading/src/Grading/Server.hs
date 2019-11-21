{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server
    ( Port
    , getPort
    , serveGrading
    ) where

import Data.Maybe               (fromMaybe)
import Network.Wai.Handler.Warp (Port, run)
import Servant

import Grading.API
import Grading.Server.GradingM
import Grading.Server.Handlers

defaultPort :: Port
defaultPort = 8080

getPort :: Maybe Port -> Port
getPort = fromMaybe defaultPort

serveGrading :: Maybe Port -> IO ()
serveGrading mport = do
    gc <- initContext
    run (getPort mport) $ gradingAppT gc

gradingAppT :: GC -> Application
gradingAppT gc = serve gradingAPI $ gradingServer gc

gradingServer :: GC -> Server GradingAPI
gradingServer gc = hoistServer gradingAPI (runGradingM gc) gradingServerT
























