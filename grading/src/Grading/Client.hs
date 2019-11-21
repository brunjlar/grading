module Grading.Client
    ( users
    , tasks
    , uploadFolder
    ) where

import Codec.Archive.Tar (pack, write)
import Codec.Compression.GZip (compress)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Grading.API
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.IO.Error (userError)

users :: ClientM [User]
tasks :: ClientM [Task]
upload :: User -> Task -> ByteString -> ClientM NoContent
users :<|> tasks :<|> upload = client gradingAPI

uploadFolder :: String -> Int -> User -> Task -> FilePath -> IO ()
uploadFolder host port user task f = do
    a <- normFolder f
    m <- newManager defaultManagerSettings
    let env = mkClientEnv m $ BaseUrl Http host port ""
    bs  <- compress . write <$> pack a ["."]
    res <- runClientM (upload user task bs) env
    case res of
        Left err        -> throwIO $ userError $ show err
        Right NoContent -> putStrLn $ "successfully uploaded " ++ show f

normFolder :: FilePath -> IO FilePath
normFolder f = do
    b <- doesDirectoryExist f
    unless b $ throwIO $ userError $ "folder " ++ show f ++ " does not exists"
    makeAbsolute f
