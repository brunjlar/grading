module TestSubmission.Submit
    ( ImageName
    , submit
    ) where

import Control.Monad         (void)
import System.FilePath       ((</>), (<.>))
import TestSubmission.Docker
import TestSubmission.Result
import UnliftIO.Temporary    (withSystemTempDirectory)

submit :: ImageName -> FilePath -> IO Result
submit n submission = withSystemTempDirectory "temp" $ \fp ->
    withDetachedContainer n (Just "/test/") $ \cid -> do
        let extractLog = fp </> "extract" <.> "log"
        let buildLog = fp </> "build" <.> "log"
        let testLog = fp </> "test" <.> "log"
        let hlintLog = fp </> "hlint" <.> "log"
        void $ copyToContainer cid submission "/test/solution.tar.gz"
        void $ execInContainer cid "./test-internal.sh"
        void $ copyFromContainer cid "/test/extract.log" extractLog
        void $ copyFromContainer cid "/test/build.log" buildLog
        void $ copyFromContainer cid "/test/test.log" testLog
        void $ copyFromContainer cid "/test/hlint.log" hlintLog
        toResult extractLog buildLog testLog hlintLog 
