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

import Grading.Server  (Port, serveGrading)

defaultPort :: Port
defaultPort = 8080

data Arg = Arg (Maybe Port <?> "port")
    deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Arg (Helpful mport) <- getRecord "Start grading server."
    let port = fromMaybe defaultPort mport
    putStrLn $ "starting server on port " ++ show port
    serveGrading port
