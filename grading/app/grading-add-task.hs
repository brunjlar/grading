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
    , image  :: String       <?> "docker image"
    , folder :: FilePath     <?> "task folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful d) (Helpful f) <- getRecord "Adds a task."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    a   <- uncheck <$> tarFolder f
    putStrLn $ "created archive (size " ++ show (uncheckedSize a) ++ ")"
    tid <- addTaskIO host' port' $ TaskDescription (DockerImage d) a
    putStrLn $ "successfully added task with id " ++ show tid
