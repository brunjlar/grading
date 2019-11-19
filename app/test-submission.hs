module Main
    ( main
    ) where

import System.Environment    (getArgs)
import TestSubmission.Submit

main :: IO ()
main = do
    [n, s] <- getArgs
    putStrLn $ "container name: " ++ n
    putStrLn $ "submission: " ++ s
    res <- submit n s
    print res
