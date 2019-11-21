{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Grading.API
    ( ByteString
    , User
    , Task
    , GradingAPI
    , gradingAPI
    ) where

import Data.ByteString.Lazy (ByteString)
import Servant
import Text.Encoding.Z (UserString)

gradingAPI :: Proxy GradingAPI
gradingAPI = Proxy

type User = UserString

type Task = Int

type GradingAPI = 
         "users" :> Get '[JSON] [User]
    :<|> "tasks" :> Get '[JSON] [Int]
    :<|> "upload" :> Capture "user" User :> Capture "task" Task :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent
