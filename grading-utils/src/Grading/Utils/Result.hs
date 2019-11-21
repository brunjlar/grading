module Grading.Utils.Result
    ( Result (..)
    ) where

import Data.Map.Strict      (Map)

import Grading.Utils.Test

data Result =
      FatalError
    | ExtractionError String
    | BuildError String
    | Tested (Either String (Map TestLabel TestResult)) (Maybe String)
    deriving (Show, Read, Eq, Ord)
