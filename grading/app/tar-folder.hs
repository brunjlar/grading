{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import qualified Data.ByteString.Lazy as B
import           Options.Generic

import Grading.Utils.Tar (tarFolder, toBS)

data Arg = Arg (FilePath <?> "folder to archive")
    deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Arg (Helpful f) <- getRecord "Create a compressed tar-archive of the specified folder and write that archive to standard output."
    checked         <- tarFolder f
    B.putStr $ toBS checked
