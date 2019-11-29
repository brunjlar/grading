{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grading.Utils.Result
    ( TestLabel
    , TestResult (..)
    , TestsAndHints (..)
    , Result (..)
    ) where

import Data.Map.Strict (Map)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

import Grading.Utils.Test

data TestsAndHints = TestsAndHints
    { thTests :: !(Either String (Map TestLabel TestResult))
    , thHints :: !(Maybe String)
    } deriving (Show, Read, Eq, Ord, Generic, Typeable)

data Result =
      FatalError
    | ExtractionError !String
    | BuildError !String
    | Tested !TestsAndHints
    deriving (Show, Read, Eq, Ord, Generic, Typeable)
