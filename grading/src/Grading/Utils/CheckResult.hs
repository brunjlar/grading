module Grading.Utils.CheckResult
    ( testedSatisfying
    , allPass
    , allFail
    ) where

import Grading.Utils.Result

testedSatisfying :: (TestsAndHints -> Bool) -> Result -> Bool
testedSatisfying check (Tested ths) = check ths
testedSatisfying _     _            = False

allPass :: TestsAndHints -> Bool
allPass ths = case thTests ths of
    Left _  -> False
    Right m -> all (== Success) m

allFail :: TestsAndHints -> Bool
allFail ths = case thTests ths of
    Left _  -> False
    Right m -> all (/= Success) m
