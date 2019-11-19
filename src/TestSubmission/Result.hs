module TestSubmission.Result
    ( Result (..)
    , toResult
    ) where

import Data.Map.Strict      (Map)
import System.Directory     (doesFileExist)
import TestSubmission.Utils
import Text.Read            (readMaybe)

data Result =
      FatalError
    | ExtractionError String
    | BuildError String
    | Tested (Either String (Map TestLabel TestResult)) (Maybe String)
    deriving (Show, Read, Eq, Ord)

toResult :: FilePath -> FilePath -> FilePath -> FilePath -> IO Result
toResult extractLog buildLog testLog hlintLog = do
    extractLogExists <- doesFileExist extractLog
    if extractLogExists then do
        buildLogExists <- doesFileExist buildLog
        if buildLogExists then do
            testLogsExist <- (&&) <$> doesFileExist testLog <*> doesFileExist hlintLog
            if testLogsExist then do
                t <- readFile testLog
                let e = case readMaybe t of
                            Nothing -> Left t
                            Just r  -> Right r
                h <- readFile hlintLog
                let m = case h of
                            "No hints\n" -> Nothing
                            hints        -> Just hints
                return $ Tested e m
            else BuildError <$> readFile buildLog
        else ExtractionError <$> readFile extractLog
    else return FatalError
