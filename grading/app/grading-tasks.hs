{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Control.Monad   (forM_)
import Data.Maybe      (fromMaybe)
import Options.Generic

import Grading.Client

data Args = Args
    { host   :: Maybe String <?> "host"
    , port   :: Maybe Int    <?> "port"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) <- getRecord "Lists all tasks."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    xs <- tasksIO host' port'
    forM_ xs print
