{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Grading.Server.GradingM
    ( GC
    , GradingM
    , MonadError (..)
    , MonadIO (..)
    , initContext
    , runGradingM
    , logMsg
    , withDB
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.Except    (MonadError (..))
import Control.Monad.Reader    (asks, MonadIO (..), ReaderT (..))
import Database.SQLite.Simple  (Connection, execute_, withConnection)
import Servant


data GC = GC
    { gcLock :: !(MVar ())
    , gcDB   :: !FilePath
    }

initContext :: IO GC
initContext = do
    lock <- newMVar ()
    let db = "db"
    withConnection db $ \conn -> do
        execute_ conn "PRAGMA foreign_keys = ON"
        execute_ conn "CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, email TEXT UNIQUE NOT NULL)" 
        execute_ conn "CREATE TABLE IF NOT EXISTS submissions (id INTEGER PRIMARY KEY AUTOINCREMENT, userid TEXT NOT NULL REFERENCES users(id), time TEXT NOT NULL, archive BLOB NOT NULL)"

    return $ GC 
        { gcLock = lock
        , gcDB   = db
        }

newtype GradingM a = GradingM (ReaderT GC Handler a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ServerError)

runGradingM :: GC -> GradingM a -> Handler a
runGradingM gc (GradingM m) = runReaderT m gc

logMsg :: String -> GradingM ()
logMsg msg = do
    lock <- GradingM $ asks gcLock
    liftIO $ withMVar lock $ \() -> putStrLn msg

withDB :: (Connection -> GradingM a) -> GradingM a
withDB f = GradingM $ ReaderT $ \gc -> do
    ea <- liftIO $ withConnection (gcDB gc) $ runHandler . runGradingM gc . f
    case ea of
        Left err -> throwError err
        Right a  -> return a
