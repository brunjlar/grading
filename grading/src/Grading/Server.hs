{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server
    ( Port
    , serveGradingT
    , serveGrading
    ) where

import Network.Wai.Handler.Warp (Port, run)
import Servant

import Grading.API
import Grading.Server.Monad.Class
import Grading.Server.Monad.GradingM
import Grading.Server.Handlers

serveGrading :: Port -> IO ()
serveGrading = serveGradingT (Proxy :: Proxy GradingM)

serveGradingT :: MonadGrading m => Proxy m -> Port -> IO ()
serveGradingT proxy port = do
    c <- initContext proxy
    let app = gradingAppT proxy c
    run port app

gradingAppT :: MonadGrading m => Proxy m -> GradingContext m -> Application
gradingAppT p c = serve gradingAPI $ gradingServer p c

gradingServer :: MonadGrading m => Proxy m -> GradingContext m -> Server GradingAPI
gradingServer p c = hoistServer gradingAPI (toHandler p c) gradingServerT
