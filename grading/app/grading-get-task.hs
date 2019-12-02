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
    { host      :: Maybe String <?> "host"
    , port      :: Maybe Int    <?> "port"
    , task      :: Int          <?> "task id"
    , taskDir   :: FilePath     <?> "task folder"
    , sampleDir :: FilePath     <?> "sample folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful tid) (Helpful tf) (Helpful sf) 
        <- getRecord "Downloads a task."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    t <- getTaskIO host' port' $ TaskId tid
    putStrLn $ "downloaded archives for task " ++ show tid
    extractArchive (tTask t) tf
    extractArchive (tSample t) sf
    putStrLn $ "extracted task archive to " ++ tf
    putStrLn $ "extracted sample archive to " ++ sf
