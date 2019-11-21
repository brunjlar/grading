module Grading.Utils.Submit
    ( ImageName
    , submit
    ) where

import           Pipes
import qualified Pipes.ByteString      as P
import           System.Directory      (removeFile)
import           System.FilePath       ((</>), (<.>))
import           System.IO             (withBinaryFile, IOMode (WriteMode))
import           UnliftIO.Temporary    (withSystemTempDirectory)

import           Grading.Utils.Docker
import           Grading.Utils.Result
import           Grading.Utils.ToResult

submit :: ImageName -> Maybe FilePath -> IO Result
submit n msubmission = withSystemTempDirectory "temp" $ \fp -> case msubmission of
    Just s  -> go fp s
    Nothing -> do
        let s = fp </> "submission" <.> "tar" <.> "gz"
        fileFromStdIn s
        res <- go fp s
        removeFile s
        return res
  where
    go :: FilePath -> FilePath -> IO Result
    go fp s = withDetachedContainer n (Just "/test/") $ \cid -> do
        let extractLog = fp </> "extract" <.> "log"
        let buildLog = fp </> "build" <.> "log"
        let testLog = fp </> "test" <.> "log"
        let hlintLog = fp </> "hlint" <.> "log"
        void $ copyToContainer cid s "/test/solution.tar.gz"
        void $ execInContainer cid "./test-internal.sh"
        void $ copyFromContainer cid "/test/extract.log" extractLog
        void $ copyFromContainer cid "/test/build.log" buildLog
        void $ copyFromContainer cid "/test/test.log" testLog
        void $ copyFromContainer cid "/test/hlint.log" hlintLog
        toResult extractLog buildLog testLog hlintLog 

fileFromStdIn :: FilePath -> IO ()
fileFromStdIn fp =
    withBinaryFile fp WriteMode $ \hOut ->
    runEffect $ P.stdin >-> P.toHandle hOut
