module Grading.Client
    ( User
    , Task
    , getPort
    , addUserIO
    , usersIO
    , addTaskIO
    , tasksIO
    , uploadIO
    ) where

import Codec.Archive.Tar      (pack, write)
import Codec.Compression.GZip (compress)
import Control.Exception      (throwIO)
import Control.Monad          (unless, void)
import Network.HTTP.Client    (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import System.Directory       (doesDirectoryExist, makeAbsolute)
import System.IO.Error        (userError)

import Grading.API
import Grading.Server         (getPort)
import Grading.Types

addUser :: UserName -> EMail -> ClientM NoContent
users   :: ClientM [User]
addTask :: DockerImage -> ClientM TaskId
tasks   :: ClientM [Task]
upload  :: UserName -> TaskId -> UncheckedArchive -> ClientM (SubmissionId, TestsAndHints)
addUser :<|> users :<|> addTask :<|> tasks :<|> upload = client gradingAPI

clientIO :: String -> Int -> ClientM a -> IO a
clientIO host port c = do
    m <- newManager defaultManagerSettings
    let env = mkClientEnv m $ BaseUrl Http host port ""
    res <- runClientM c env
    case res of
        Left err -> throwIO $ userError $ show err
        Right a  -> return a

addUserIO :: String -> Int -> User -> IO ()
addUserIO host port u = void $ clientIO host port $ addUser (userName u) (userEMail u)

usersIO :: String -> Int -> IO [User]
usersIO host port = clientIO host port users

addTaskIO :: String -> Int -> DockerImage -> IO TaskId
addTaskIO host port d = clientIO host port $ addTask d

tasksIO :: String -> Int -> IO [Task]
tasksIO host port = clientIO host port tasks

uploadIO :: String -> Int -> UserName -> TaskId -> FilePath -> IO (SubmissionId, TestsAndHints)
uploadIO host port n tid fp = do
    nfp       <- normFolder fp
    unchecked <- UncheckedArchive . compress . write <$> pack nfp ["."]
    clientIO host port $ upload n tid unchecked

normFolder :: FilePath -> IO FilePath
normFolder f = do
    b <- doesDirectoryExist f
    unless b $ throwIO $ userError $ "folder " ++ show f ++ " does not exists"
    makeAbsolute f
