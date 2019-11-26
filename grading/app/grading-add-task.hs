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

data Args = Args
    { host  :: Maybe String <?> "host"
    , port  :: Maybe Int    <?> "port"
    , image :: String       <?> "docker image"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful d) <- getRecord "Adds a task."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    tid <- addTaskIO host' port' $ DockerImage d
    putStrLn $ "successfully added task with id " ++ show tid
