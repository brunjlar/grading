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
import Text.Printf     (printf)

import Grading.Client
import Grading.Types

data Args = Args
    { host     :: Maybe String <?> "host"
    , port     :: Maybe Int    <?> "port"
    , user     :: String       <?> "username"
    , password :: String       <?> "password"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful pw) <- getRecord "Lists all users."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    xs <- usersIO host' port' (UserName n) (Password pw)
    forM_ xs $ \u -> printf "%-20s %-30s %-10s\n" (userName u) (userEMail u) (show $ userRole u)
