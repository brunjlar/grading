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
    , withDBIO
    , withDB
    , isAdmin
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.Except    (MonadError (..))
import Control.Monad.Reader    (asks, MonadIO (..), ReaderT (..))
import Database.SQLite.Simple  (Connection, execute_, withConnection)
import Servant

import Grading.Types

data GC = GC
    { gcLock   :: !(MVar ())
    , gcDB     :: !FilePath
    , gcAdmins :: ![UserName]
    }

initContext :: [UserName] -> IO GC
initContext admins = do
    lock <- newMVar ()
    let db = "db"
    withConnection db $ \conn -> do
        execute_ conn "CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, email TEXT UNIQUE NOT NULL, role TEXT NOT NULL, salt TEXT NOT NULL, hash TEXT NOT NULL)" 
        execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY AUTOINCREMENT, image TEXT UNIQUE NOT NULL, task BLOB NOT NULL, sample BLOB NOT NULL)" 
        execute_ conn "CREATE TABLE IF NOT EXISTS submissions (id INTEGER PRIMARY KEY AUTOINCREMENT, userid TEXT NOT NULL REFERENCES users(id), taskid INTEGER NOT NULL REFERENCES tasks(id), time TEXT NOT NULL, archive BLOB NULL, result TEXT NOT NULL, remark TEXT NULL"
    return $ GC 
        { gcLock   = lock
        , gcDB     = db
        , gcAdmins = admins
        }

newtype GradingM a = GradingM (ReaderT GC Handler a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ServerError)

runGradingM :: GC -> GradingM a -> Handler a
runGradingM gc (GradingM m) = runReaderT m gc

logMsg :: String -> GradingM ()
logMsg msg = do
    lock <- GradingM $ asks gcLock
    liftIO $ withMVar lock $ \() -> putStrLn msg

withDBIO :: GC -> (Connection -> IO a) -> IO a
withDBIO gc f = withConnection (gcDB gc) $ \conn -> do
    execute_ conn "PRAGMA foreign_keys = ON"
    f conn

withDB :: (Connection -> GradingM a) -> GradingM a
withDB f = GradingM $ ReaderT $ \gc -> do
    ea <- liftIO $ withDBIO gc $ runHandler . runGradingM gc . f
    case ea of
        Left err -> throwError err
        Right a  -> return a

isAdmin :: UserName -> GradingM Bool
isAdmin n = GradingM $ ReaderT $ \gc -> return $ n `elem` gcAdmins gc
