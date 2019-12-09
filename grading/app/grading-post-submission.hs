{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import           Data.Maybe           (fromMaybe)
import           Options.Generic
import           Text.Printf          (printf)

import           Grading.Client
import           Grading.Types
import           Grading.Utils.Report

data Args = Args
    { host     :: Maybe String <?> "host"
    , port     :: Maybe Int    <?> "port"
    , user     :: String       <?> "username"
    , password :: String       <?> "password"
    , task     :: Int          <?> "task id"
    , folder   :: FilePath     <?> "submission folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful pw) (Helpful tid) (Helpful folder')
        <- getRecord "Uploads a submission to the grading server."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    printf "uploading submission folder '%s' for task %d to %s:%d for user '%s'"
        folder' tid host' port' n

    sub <- postSubmissionIO host' port' (UserName n) (Password pw) (TaskId tid) folder'

    putStrLn "uploaded submission"
    reportSubmission sub
