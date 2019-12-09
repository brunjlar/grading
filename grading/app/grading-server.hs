{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Control.Monad   (forM_)
import Options.Generic

import Grading.Server
import Grading.Types

data Arg = Arg { port   :: Maybe Port <?> "port"
               , admins :: [String]   <?> "admins"
               }
    deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Arg (Helpful mport) (Helpful xs) <- getRecord "Start grading server."
    let madmins = case xs of
            []      -> Nothing
            (_ : _) -> Just $ UserName <$> xs
    putStrLn $ "starting server on port " ++ show (getPort mport)
    putStrLn "admins:"
    forM_ (getAdmins madmins) $ \admin -> putStr " - " >> print admin
    serveGrading mport madmins
