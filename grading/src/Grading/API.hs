{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Grading.API
    ( ByteString
    , GradingAPI
    , gradingAPI
    ) where

import Data.ByteString.Lazy             (ByteString)
import Servant

import Grading.Types

gradingAPI :: Proxy GradingAPI
gradingAPI = Proxy

type GradingAPI = 
         "user"   :> Capture "user" UserName :> ReqBody '[JSON] EMail :> Put '[JSON] NoContent
    :<|> "users"  :> Get '[JSON] [User]
    :<|> "tasks"  :> Get '[JSON] [Int]
    :<|> "upload" :> Capture "user" UserName :> Capture "task" Task :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent
