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

gradingAPI :: Proxy GradingAPI
gradingAPI = Proxy

type User = String

type Task = Int

type GradingAPI = 
         "users"  :> Get '[JSON] [User]
    :<|> "tasks"  :> Get '[JSON] [Int]
    :<|> "upload" :> Capture "user" User :> Capture "task" Task :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent

