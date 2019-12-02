module Grading.Utils.Report
    ( reportSubmission
    , reportResult
    ) where

import           Control.Monad        (forM_)
import qualified Data.Map.Strict      as M
import           Text.Printf          (printf)

import           Grading.Submission
import           Grading.Utils.Result

reportSubmission :: Submission -> IO ()
reportSubmission sub = do
    printf "id: %s\n"   $ show $ subId   sub
    printf "user: %s\n" $ show $ subUser sub
    printf "task: %s\n" $ show $ subTask sub
    printf "time: %s\n" $ show $ subTime sub
    reportResult $ subResult sub

reportResult :: Result -> IO ()
reportResult res = case res of

    Tested (TestsAndHints em mh) -> do
        putStrLn "test results:"
        case em of
            Left err -> putStrLn $ "ERROR: " ++ err
            Right m  -> forM_ (M.toList m) $ \(l, r) -> do
                putStr $ l ++ ": "
                putStrLn $ case r of
                    Success         -> "Success"
                    Failure _ o _ _ -> o
        case mh of
            Nothing -> return ()
            Just h  -> printf "\nhints:\n%s\n" h 

    FatalError -> putStrLn "fatal error"

    ExtractionError e -> printf "error during extraction:\n%s\n" e

    BuildError e -> printf "build error:\n%s\n" e
