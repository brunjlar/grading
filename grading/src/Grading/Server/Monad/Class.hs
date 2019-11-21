{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Grading.Server.Monad.Class
    ( Connection
    , MonadGrading (..)
    ) where

import Control.Monad.Except (MonadError (..), MonadIO (..))
import Database.SQLite.Simple (Connection)
import Servant

import Grading.API

class (MonadError ServerError m, MonadIO m) => MonadGrading m where
    type GradingContext m :: *
    initContext :: Proxy m -> IO (GradingContext m)
    toHandler :: Proxy m -> GradingContext m -> m a -> Handler a
    logMsg :: String -> m ()
    extractFolder :: FilePath -> ByteString -> m ()
    withDB :: (Connection -> m a) -> m a
