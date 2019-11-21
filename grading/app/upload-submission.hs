module Main
    ( main
    ) where

import Grading.Client (uploadFolder)
import System.Environment (getArgs)

main :: IO ()
main = do
    [port', user, task', folder] <- getArgs
    let port = read port'
        task = read task'
    putStrLn $ "uploading folder '" ++ folder ++ "' to port " ++ show port ++ " for user '" ++ user ++ "' and task " ++ show task
    uploadFolder "127.0.0.1" port user task folder
