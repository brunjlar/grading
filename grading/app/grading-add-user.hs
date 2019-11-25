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
    { host   :: Maybe String <?> "host"
    , port   :: Maybe Int    <?> "port"
    , user   :: String       <?> "username"
    , email  :: String       <?> "email"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful e) <- getRecord "Adds a user."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
        u     = User (UserName n) (EMail e)
    addUserIO host' port' u
    putStrLn $ "successfully added user " ++ show u
