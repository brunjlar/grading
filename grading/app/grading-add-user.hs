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
    { host     :: Maybe String <?> "host"
    , port     :: Maybe Int    <?> "port"
    , user     :: String       <?> "username"
    , email    :: String       <?> "email"
    , password :: String       <?> "password"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful e) (Helpful pw) <- getRecord "Adds a user."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    addUserIO host' port' (UserName n) (EMail e) (Password pw)
    putStrLn $ "successfully added user " ++ show n
