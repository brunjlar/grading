{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module TestSubmission.Utils
    ( TestCase (..)
    , label
    , testTC
    , testTCs
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck hiding (label, Result (..))
import qualified Test.QuickCheck as QC

data TestCase where
    TC  :: Testable prop => String -> prop -> TestCase
    TCs :: String -> [TestCase] -> TestCase

label :: TestCase -> String
label (TC s _)  = s
label (TCs s _) = s

data Result = 
      Success
    | Failure { resLabels    :: [String]
              , resOutput    :: String
              , resCases     :: [String]
              , resException :: Maybe String
              }
    deriving (Show, Read, Eq, Ord)

testTC :: Int -> TestCase -> IO Result
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

testTCs :: Int -> [TestCase] -> IO (Map String Result)
testTCs timeout = fmap Map.fromList . mapM (\tc -> (label tc,) <$> testTC timeout tc)
