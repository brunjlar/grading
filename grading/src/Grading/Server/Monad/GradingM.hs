{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Grading.Server.Monad.GradingM
    ( GradingM
    ) where

import           Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           Control.Exception (Exception (..), SomeException, try)
import           Control.Monad.Except (MonadError (..), MonadIO (..))
import           Control.Monad.Reader (asks, ReaderT (..))
import           Database.SQLite.Simple (execute_, withConnection)
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Data.Foldable (asum)
import           Prelude hiding (log)
import           Servant
import           System.Directory (createDirectoryIfMissing, removePathForcibly)

import           Grading.API
import           Grading.Server.Monad.Class

data GC = GC
    { gcLock :: !(MVar ())
    , gcDB   :: !FilePath
    }

newtype GradingM a = GradingM (ReaderT GC Handler a)
    deriving (Functor, Applicative, Monad, MonadError ServerError, MonadIO)

runGradingM :: GC -> GradingM a -> IO (Either ServerError a)
runGradingM gc = runHandler . toHandler Proxy gc

instance MonadGrading GradingM where

    type GradingContext GradingM = GC

    initContext Proxy = do
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

    toHandler _ c (GradingM m) = runReaderT m c

    logMsg msg = do
        lock <- GradingM $ asks gcLock
        liftIO $ withMVar lock $ \() -> putStrLn msg

    extractFolder f bs = do
        e <- liftIO $ try $ do
                removePathForcibly f
                createDirectoryIfMissing True f
                Tar.unpack f $ checkedRead $ decompress bs
        case e of
            Left (err :: SomeException) -> throwError err400 {errBody = fromString $ displayException err}
            Right ()                    -> return ()

    withDB f = GradingM $ ReaderT $ \gc -> do
        ea <- liftIO $ withConnection (gcDB gc) $ runGradingM gc . f
        case ea of
            Left err -> throwError err
            Right a  -> return a

checkedRead :: ByteString -> Tar.Entries TarError
checkedRead = fmap toTarError . Tar.checkSecurity . Tar.checkPortability . Tar.checkTarbomb "." . Tar.read

data TarError =
      FormatError !Tar.FormatError
    | TarBombError !Tar.TarBombError
    | PortabilityError !Tar.PortabilityError
    | FileNameError !Tar.FileNameError
    deriving Show

toTarError :: Either (Either (Either Tar.FormatError Tar.TarBombError) Tar.PortabilityError) Tar.FileNameError -> TarError
toTarError (Right e)               = FileNameError e
toTarError (Left (Right e))        = PortabilityError e
toTarError (Left (Left (Right e))) = TarBombError e
toTarError (Left (Left (Left e)))  = FormatError e

instance Exception TarError where

    toException (FormatError e)      = toException e
    toException (TarBombError e)     = toException e
    toException (PortabilityError e) = toException e
    toException (FileNameError e)    = toException e

    fromException e = asum $ map ($ e) 
        [ fmap FormatError . fromException
        , fmap TarBombError . fromException
        , fmap PortabilityError . fromException
        , fmap FileNameError . fromException
        ]
    
    displayException (FormatError e)      = displayException e 
    displayException (TarBombError e)     = displayException e 
    displayException (PortabilityError e) = displayException e 
    displayException (FileNameError e)    = displayException e 
