module Main
    ( main
    ) where

import Grading.Server (Port, serveGrading)
import System.Environment (getArgs)
import Text.Read (readMaybe)

defaultPort :: Port
defaultPort = 8080

main :: IO ()
main = do
    xs <- getArgs 
    case xs of
        [s] -> case readMaybe s of
            Just port -> runOnPort port
            Nothing   -> usage
        []  -> runOnPort defaultPort
        _   -> usage
  where
    usage = putStrLn "USAGE: grading [PORT]"

runOnPort :: Port -> IO ()
runOnPort port = do
    putStrLn $ "starting server on port " ++ show port
    serveGrading port
