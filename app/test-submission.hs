{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Options.Generic

import TestSubmission.Submit

data Args = Args 
    { image   :: ImageName <?> "docker image name"
    , archive :: FilePath  <?> "path to submission archive"
    } deriving (Show, Generic)

instance ParseRecord Args where
    parseRecord = parseRecordWithModifiers defaultModifiers {shortNameModifier = firstLetter}

main :: IO ()
main = do
    Args (Helpful n) (Helpful s) <- getRecord "Tests a submission and writes the result to standard output."
    res <- submit n s
    print res
