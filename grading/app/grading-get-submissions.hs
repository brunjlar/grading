{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Control.Monad        (forM_)
import Data.Maybe           (fromMaybe)
import Options.Generic
import Text.Printf          (printf)

import Grading.Client
import Grading.Types
import Grading.Utils.Report

data Args = Args
    { host       :: Maybe String <?> "host"
    , port       :: Maybe Int    <?> "port"
    , admin      :: String       <?> "admin name"
    , password   :: String       <?> "admin password"
    , user       :: String       <?> "username"
    , task       :: Int          <?> "task"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful na) (Helpful pw) (Helpful n) (Helpful tid) <- getRecord "Lists all submissions of a user for a task."
    printf "listing all submissions of user '%s' for task %d\n" n tid
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    subs <- getSubmissionsIO host' port' (UserName na) (Password pw) (UserName n) (TaskId tid)
    printf "downloaded %d submission(s)" $ length subs
    forM_ subs $ \sub -> reportSubmission sub >> putStrLn ""
