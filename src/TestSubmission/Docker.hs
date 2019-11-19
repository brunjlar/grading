module TestSubmission.Docker
    ( ContainerName
    , ContainerId
    , ExitCode
    , runDetachedContainer
    , stopContainer
    , withDetachedContainer
    , execInContainer
    , copyToContainer
    , copyFromContainer
    ) where

import           Control.Exception          (bracket)
import           Control.Monad              (void)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String                (IsString (..))
import           GHC.IO.Exception           (ExitCode)
import           System.Process.Typed

type ContainerName = String

newtype ContainerId = ContainerId String
    deriving (Show, Read, Eq, Ord)

runDetachedContainer :: ContainerName -> Maybe FilePath -> IO ContainerId
runDetachedContainer n workDir = do
    let w = case workDir of
                Nothing -> ""
                Just d  -> " -w " ++ d
    (res, _) <- readProcess_ $ fromString $ "docker run -dit" ++ w ++ " " ++ n
    return $ ContainerId $ BS.unpack $ BS.takeWhile (/= '\n') res

stopContainer :: ContainerId -> IO ()
stopContainer (ContainerId cid) = void $ readProcess_ $ fromString $ "docker stop " ++ cid

withDetachedContainer :: ContainerName -> Maybe FilePath -> (ContainerId -> IO a) -> IO a
withDetachedContainer n workDir act = bracket
    (runDetachedContainer n workDir)
    stopContainer
    act

execInContainer :: ContainerId -> String -> IO ExitCode
execInContainer (ContainerId cid) command = do
    (e, _, _) <- readProcess $ fromString $ "docker exec -it " ++ cid ++ " " ++ command
    return e

copyToContainer :: ContainerId -> FilePath -> FilePath -> IO ExitCode
copyToContainer (ContainerId cid) from to =
    runProcess $ fromString $ "docker cp " ++ from ++ " " ++ cid ++ ":" ++ to

copyFromContainer :: ContainerId -> FilePath -> FilePath -> IO ExitCode
copyFromContainer (ContainerId cid) from to = runProcess $ fromString $ "docker cp " ++ cid ++ ":" ++ from ++ " " ++ to
