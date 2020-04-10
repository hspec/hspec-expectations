{-# LANGUAGE FlexibleContexts #-}

module Helper where

import           Test.HUnit.Lang
import           Data.CallStack

-- note: this function expects the callstacks to have identical line
-- numbers, so you need to be careful to call expectationFailed on the same
-- line that you throw any exceptions
expectationFailed :: HasCallStack => FailureReason -> HUnitFailure -> Bool
expectationFailed msg (HUnitFailure l m) = m == msg && (fmap setColumn l) == (fmap setColumn location)
  where
    location = case reverse callStack of
      [] -> Nothing
      (_, loc) : _ -> Just loc
    location :: Maybe SrcLoc

    setColumn loc_ = loc_{srcLocStartCol = 0, srcLocEndCol = 0}
