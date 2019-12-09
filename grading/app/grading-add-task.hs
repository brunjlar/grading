{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Data.Maybe      (fromMaybe)
import Options.Generic

import Grading.Client
import Grading.Types
import Grading.Utils.Tar

data Args = Args
    { host     :: Maybe String <?> "host"
    , port     :: Maybe Int    <?> "port"
    , user     :: String       <?> "username"
    , password :: String       <?> "password"
    , image    :: String       <?> "docker image"
    , task     :: FilePath     <?> "task folder"
    , sample   :: FilePath     <?> "sample solution folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful pw) (Helpful d) (Helpful tf) (Helpful sf) <- getRecord "Adds a task."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    ta <- uncheck <$> tarFolder tf
    sa <- uncheck <$> tarFolder sf
    putStrLn $ "created task archive (size " ++ show (archiveSize ta) ++ ")"
    putStrLn $ "created sample archive (size " ++ show (archiveSize sa) ++ ")"
    let t = Task
                { tId     = NotRequired
                , tImage  = DockerImage d
                , tTask   = ta
                , tSample = sa
                }
    tid <- addTaskIO host' port' (UserName n) (Password pw) t
    putStrLn $ "successfully added task with id " ++ show tid
