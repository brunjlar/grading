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
                                            "user"        :> Capture "user" UserName :> ReqBody '[JSON] (EMail, Password) :> Put '[JSON] NoContent
    :<|> BasicAuth "admin" Administrator :> "users"       :> Get '[JSON] [User]
    :<|> BasicAuth "admin" Administrator :> "task"        :> ReqBody '[OctetStream] (Task Unchecked) :> Post '[JSON] TaskId
    :<|> BasicAuth "user"  User          :> "task"        :> Capture "task" TaskId :> Capture "withSample" Bool :> Get '[OctetStream] (Task Checked)
    :<|> BasicAuth "admin" Administrator :> "submission"  :> Capture "submission" SubmissionId :> Get '[OctetStream] (Submission Checked)
    :<|> BasicAuth "user"  User          :> "submission"  :> ReqBody '[OctetStream] (Submission Unchecked) :> Post '[OctetStream] (Submission Checked)
    :<|> BasicAuth "admin" Administrator :> "submissions" :> Capture "user" UserName :> Capture "task" TaskId :> Get '[OctetStream] [Submission Checked]
