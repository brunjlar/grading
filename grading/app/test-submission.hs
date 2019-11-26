{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Options.Generic

import Grading.Types
import Grading.Utils.Submit

data Args = Args 
    { image   :: String         <?> "docker image name"
    , archive :: Maybe FilePath <?> "path to submission archive"
    } deriving (Show, Generic)

instance ParseRecord Args where
    parseRecord = parseRecordWithModifiers defaultModifiers {shortNameModifier = firstLetter}

main :: IO ()
main = do
    Args (Helpful n) (Helpful ms) <- getRecord "Tests a submission and writes the result to standard output."
    res <- submit (DockerImage n) ms
    print res
