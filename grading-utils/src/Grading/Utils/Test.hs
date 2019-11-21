{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Grading.Utils.Test
    ( TestCase (..)
    , TestLabel
    , TestResult (..)
    , getTestLabel
    , testTC
    , testTCs
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck hiding (label, Result (..))
import qualified Test.QuickCheck as QC

type TestLabel = String

data TestCase where
    TC  :: Testable prop => TestLabel -> prop -> TestCase
    TCs :: TestLabel -> [TestCase] -> TestCase

getTestLabel :: TestCase -> String
getTestLabel (TC s _)  = s
getTestLabel (TCs s _) = s

data TestResult = 
      Success
    | Failure { resLabels    :: [String]
              , resOutput    :: String
              , resCases     :: [String]
              , resException :: Maybe String
              }
    deriving (Show, Read, Eq, Ord)

testTC :: Int -> TestCase -> IO TestResult
testTC timeout (TC l prop) = do
    res <- quickCheckWithResult stdArgs{chatty = False} $ within timeout prop
    return $ case res of
        QC.Success _ _ _ _ _ _                 -> Success
        QC.GaveUp _ _ _ _ _ o                  -> Failure [l] o [] Nothing
        QC.Failure _ _ _ _ _ _ _ _ ex o cs _ _ -> Failure [l] o cs (show <$> ex)
        QC.NoExpectedFailure _ _ _ _ _ o       -> Failure [l] o [] Nothing
testTC timeout (TCs l xs) = go xs
  where
    go []       = return Success
    go (y : ys) = do
        r <- testTC timeout y
        case r of
            Success            -> go ys
            Failure ls o cs ex -> return $ Failure (l : ls) o cs ex

testTCs :: Int -> [TestCase] -> IO (Map TestLabel TestResult)
testTCs timeout = fmap Map.fromList . mapM (\tc -> (getTestLabel tc,) <$> testTC timeout tc)
