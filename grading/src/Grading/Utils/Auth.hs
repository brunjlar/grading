{-# LANGUAGE OverloadedStrings #-}

module Grading.Utils.Auth
    ( toAuthData
    , checkAdmin
    ) where

import qualified Data.Binary          as B
import qualified Data.ByteString      as SB
import qualified Data.ByteString.Lazy as LB
import           Database.SQLite.Simple
import           Servant

import           Grading.Server.GradingM (GC, withDBIO)
import           Grading.Types
import           Grading.Utils.Crypto

toAuthData :: UserName -> Password -> BasicAuthData
toAuthData n pw = BasicAuthData (toBS n) (toBS pw)
  where
    toBS :: B.Binary a => a -> SB.ByteString
    toBS = LB.toStrict . B.encode

checkAdmin :: GC -> BasicAuthCheck Administrator
checkAdmin gc = BasicAuthCheck $ \bad -> do
    res <- unBasicAuthCheck (checkUser gc) bad
    return $ case res of
        Unauthorized -> Unauthorized
        BadPassword  -> BadPassword
        NoSuchUser   -> NoSuchUser
        Authorized u
            | userRole u == Admin -> Authorized (administrator u)
            | otherwise           -> Unauthorized

checkUser :: GC -> BasicAuthCheck User
checkUser gc = BasicAuthCheck $ \bad -> case fromAuthData bad of
    Nothing      -> return NoSuchUser
    Just (n, pw) -> do
        xs <- withDBIO gc $ \conn -> query conn "SELECT * FROM users where id=?" (Only n)
        return $ case xs of
            []      -> NoSuchUser
            (u : _) -> if hash pw (userSalt u) == userHash u
                then Authorized u
                else BadPassword

fromAuthData :: BasicAuthData -> Maybe (UserName, Password)
fromAuthData bad = do
    n  <- fromBS $ basicAuthUsername bad
    pw <- fromBS $ basicAuthPassword bad
    return (n, pw)
  where
    fromBS :: B.Binary a => SB.ByteString -> Maybe a
    fromBS bs = case B.decodeOrFail $ LB.fromStrict bs of
        Left _          -> Nothing
        Right (_, _, a) -> Just a

