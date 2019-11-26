module Grading.Utils.Docker
    ( ExitCode
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

import           Grading.Types

runDetachedContainer :: DockerImage -> Maybe FilePath -> IO ContainerId
runDetachedContainer (DockerImage n) workDir = do
    let w = case workDir of
                Nothing -> ""
                Just d  -> " -w " ++ d
    (res, _) <- readProcess_ $ fromString $ "docker run -dt" ++ w ++ " " ++ n
    return $ ContainerId $ BS.unpack $ BS.takeWhile (/= '\n') res

stopContainer :: ContainerId -> IO ()
stopContainer (ContainerId cid) = void $ readProcess_ $ fromString $ "docker stop " ++ cid

withDetachedContainer :: DockerImage -> Maybe FilePath -> (ContainerId -> IO a) -> IO a
withDetachedContainer n workDir act = bracket
    (runDetachedContainer n workDir)
    stopContainer
    act

execInContainer :: ContainerId -> String -> IO ExitCode
execInContainer (ContainerId cid) command =
    runProcessSuppressOutput $ fromString $ "docker exec -t " ++ cid ++ " " ++ command

copyToContainer :: ContainerId -> FilePath -> FilePath -> IO ExitCode
copyToContainer (ContainerId cid) from to =
    runProcessSuppressOutput $ fromString $ "docker cp " ++ from ++ " " ++ cid ++ ":" ++ to

copyFromContainer :: ContainerId -> FilePath -> FilePath -> IO ExitCode
copyFromContainer (ContainerId cid) from to = 
    runProcessSuppressOutput $ fromString $ "docker cp " ++ cid ++ ":" ++ from ++ " " ++ to

runProcessSuppressOutput :: ProcessConfig stdin stdoutIgnored stderrIgnored -> IO ExitCode
runProcessSuppressOutput p = do
    (e, _, _) <- readProcess p
    return e
