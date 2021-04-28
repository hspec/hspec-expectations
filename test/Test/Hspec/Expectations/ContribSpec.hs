module Test.Hspec.Expectations.ContribSpec (spec) where

import           Test.Hspec (Spec, describe, it)

import           Test.Hspec.Expectations hiding (HasCallStack)
import           Test.Hspec.Expectations.Contrib
import           Test.HUnit.Lang
import           Data.CallStack
import qualified Data.Set as S


expectationFailed :: HasCallStack => FailureReason -> HUnitFailure -> Bool
expectationFailed msg (HUnitFailure l m) = m == msg && (fmap setColumn l) == (fmap setColumn location)
  where
    location = case reverse callStack of
      [] -> Nothing
      (_, loc) : _ -> Just loc
    location :: Maybe SrcLoc

    setColumn loc_ = loc_{srcLocStartCol = 0, srcLocEndCol = 0}


one :: Int
one = 1

spec :: Spec
spec = do
  describe "shouldInclude" $ do
    it "fails for an empty list" $ do
      ([] `shouldInclude` one) `shouldThrow` expectationFailed (Reason "[] does not include 1")

    it "succeeds for a single item list" $ do
      [one] `shouldInclude` one

    it "succeeds for a longer list" $ do
      [1, 2, 2, 3] `shouldInclude` one

    it "succeeds with repeated inclusion" $ do
      [1, 2, 1] `shouldInclude` one

  describe "shouldIncludeAll" $ do
    it "should pass for lists in order" $
        [1 :: Int, 2, 3] `shouldIncludeAll` [1, 3]

    it "should fail with a nice message for lists with extra item" $
        ([one, 3] `shouldIncludeAll` [1, 2, 3]) `shouldThrow` expectationFailed (Reason "[1,3] did not include all of [1,2,3] - missing: 2")

    it "should pass for lists out of order" $
        [0, one, 2, 3] `shouldIncludeAll` [3, 2, 0, 1]

    it "should pass for sets out of order" $
        S.fromList [0 :: Int, 1, 2] `shouldIncludeAll` [2, 1]
