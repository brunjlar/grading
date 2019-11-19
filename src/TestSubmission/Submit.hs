{-# LANGUAGE ScopedTypeVariables #-}

module TestSubmission.Submit
    ( submit
    ) where

import Control.Exception (try, SomeException)
import System.FilePath ((</>), (<.>))
import TestSubmission.Docker
import TestSubmission.Result
import UnliftIO.Temporary (withSystemTempDirectory)

submit :: ContainerName -> FilePath -> IO Result
submit n submission = withSystemTempDirectory "temp" $ \fp ->
    withDetachedContainer n (Just "/test/") $ \cid -> do
        let extractLog = fp </> "extract" <.> "log"
        let buildLog = fp </> "build" <.> "log"
        let testLog = fp </> "test" <.> "log"
        let hlintLog = fp </> "hlint" <.> "log"
        copyToContainer cid submission "/test/solution.tar.gz"
        e <- try $ do
            execInContainer cid "./test-internal.sh"
            copyFromContainer cid "/test/extract.log" extractLog
            copyFromContainer cid "/test/build.log" buildLog
            copyFromContainer cid "/test/test.log" testLog
            copyFromContainer cid "/test/hlint.log" hlintLog
        case e of
            Left (ex :: SomeException) -> putStrLn $ "EXCEPTION: " ++ show ex
            Right ()                   -> putStrLn "OK"
        toResult extractLog buildLog testLog hlintLog 
