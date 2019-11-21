{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Options.Generic

import Grading.Server

data Arg = Arg (Maybe Port <?> "port")
    deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Arg (Helpful mport) <- getRecord "Start grading server."
    putStrLn $ "starting server on port " ++ show (getPort mport)
    serveGrading mport
