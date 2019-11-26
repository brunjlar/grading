{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import           Control.Monad   (forM_)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Options.Generic
import           Text.Printf     (printf)

import           Grading.Client
import           Grading.Types

data Args = Args
    { host   :: Maybe String <?> "host"
    , port   :: Maybe Int    <?> "port"
    , user   :: String       <?> "username"
    , task   :: Int          <?> "task id"
    , folder :: FilePath     <?> "submission folder"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args (Helpful mhost) (Helpful mport) (Helpful n) (Helpful tid) (Helpful folder')
        <- getRecord "Uploads a submission to the grading server."
    let host' = fromMaybe "127.0.0.1" mhost
        port' = getPort mport
    printf "uploading submission folder '%s' for task %d to %s:%d for user '%s'"
        folder' tid host' port' n

    (sid, TestsAndHints em mh) <- uploadIO host' port' (UserName n) (TaskId tid) folder'

    printf "successfully uploaded submission %s\n\n" $ show sid
    putStrLn "test results:"
    case em of
        Left err -> putStrLn $ "ERROR: " ++ err
        Right m  -> forM_ (M.toList m) $ \(l, r) -> do
            putStr $ l ++ ": "
            putStrLn $ case r of
                Success         -> "Success"
                Failure _ o _ _ -> o
    case mh of
        Nothing -> return ()
        Just h  -> printf "\nhints:\n%s\n" h 
