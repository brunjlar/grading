{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.API
    ( ByteString
    , GradingAPI
    , gradingAPI
    ) where

import Data.ByteString.Lazy    (ByteString)
import Servant               

import Grading.Submission      (Submission)
import Grading.Types
import Grading.Utils.Tar

gradingAPI :: Proxy GradingAPI
gradingAPI = Proxy

type GradingAPI = 
         "user"       :> Capture "user" UserName :> ReqBody '[JSON] EMail :> Put '[JSON] NoContent
    :<|> "users"      :> Get '[JSON] [User]
    :<|> "task"       :> ReqBody '[OctetStream] (Task Unchecked) :> Post '[JSON] TaskId
    :<|> "task"       :> Capture "task" TaskId :> Get '[OctetStream] (Task Checked)
    :<|> "submission" :> Capture "submission" SubmissionId :> Get '[OctetStream] (Submission Checked)
    :<|> "submission" :> ReqBody '[OctetStream] (Submission Unchecked) :> Post '[OctetStream] (Submission Checked)
