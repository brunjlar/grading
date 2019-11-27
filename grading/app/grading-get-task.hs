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
    { host   :: Maybe String <?> "host"
    , port   :: Maybe Int    <?> "port"
    , task   :: Int          <?> "task id"
    , folder :: FilePath     <?> "folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful tid) (Helpful f) <- getRecord "Downloads a task."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    a <- getTaskIO host' port' $ TaskId tid
    putStrLn $ "downloaded archive for task " ++ show tid
    extractArchive a f
    putStrLn $ "extracted archive to " ++ f
