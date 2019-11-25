module Grading.Client
    ( User
    , Task
    , getPort
    , addUserIO
    , users
    , tasks
    , listUsers
    , uploadFolder
    ) where

import Codec.Archive.Tar (pack, write)
import Codec.Compression.GZip (compress)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.IO.Error (userError)

import Grading.API
import Grading.Server (getPort)
import Grading.Types

addUser :: UserName -> EMail -> ClientM NoContent
users   :: ClientM [User]
tasks   :: ClientM [Task]
upload  :: UserName -> Task -> ByteString -> ClientM NoContent
addUser :<|> users :<|> tasks :<|> upload = client gradingAPI

addUserIO :: String -> Int -> UserName -> EMail -> IO ()
addUserIO host port un email = do
    m <- newManager defaultManagerSettings
    let env = mkClientEnv m $ BaseUrl Http host port ""
    res <- runClientM (addUser un email) env
    case res of
        Left err        -> throwIO $ userError $ show err
        Right NoContent -> putStrLn $ "successfully added user '" ++ show un ++ "'"

listUsers :: String -> Int -> IO [User]
listUsers host port = do
    m <- newManager defaultManagerSettings
    let env = mkClientEnv m $ BaseUrl Http host port ""
    res <- runClientM users env
    case res of
        Left err -> throwIO $ userError $ show err
        Right xs -> return xs

uploadFolder :: String -> Int -> UserName -> Task -> FilePath -> IO ()
uploadFolder host port un task f = do
    a <- normFolder f
    m <- newManager defaultManagerSettings
    let env = mkClientEnv m $ BaseUrl Http host port ""
    bs  <- compress . write <$> pack a ["."]
    res <- runClientM (upload un task bs) env
    case res of
        Left err        -> throwIO $ userError $ show err
        Right NoContent -> putStrLn $ "successfully uploaded " ++ show f

normFolder :: FilePath -> IO FilePath
normFolder f = do
    b <- doesDirectoryExist f
    unless b $ throwIO $ userError $ "folder " ++ show f ++ " does not exists"
    makeAbsolute f















