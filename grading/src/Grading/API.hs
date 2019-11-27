{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Grading.API
    ( ByteString
    , GradingAPI
    , gradingAPI
    ) where

import Data.ByteString.Lazy    (ByteString)
import Servant               

import Grading.Types
import Grading.Utils.Tar

gradingAPI :: Proxy GradingAPI
gradingAPI = Proxy

type GradingAPI = 
         "user"   :> Capture "user" UserName :> ReqBody '[JSON] EMail :> Put '[JSON] NoContent
    :<|> "users"  :> Get '[JSON] [User]
    :<|> "task"   :> ReqBody '[OctetStream] TaskDescription :> Post '[JSON] TaskId
    :<|> "task"   :> Capture "task" TaskId :> Get '[OctetStream] CheckedArchive
    :<|> "upload" :> Capture "user" UserName :> Capture "task" TaskId :> ReqBody '[OctetStream] UncheckedArchive :> Post '[JSON] (SubmissionId, TestsAndHints) 









































