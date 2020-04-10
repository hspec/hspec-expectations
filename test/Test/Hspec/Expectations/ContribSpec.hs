{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Hspec.Expectations.ContribSpec (spec) where

import           Test.HUnit.Lang
import           Test.Hspec (Spec, describe, it)

import           Test.Hspec.Expectations hiding (HasCallStack)
import           Test.Hspec.Expectations.Contrib
import           Helper

spec :: Spec
spec = do
  describe "annotate" $ do
    it "does not affect the running of a successful test" $ do
      annotate "obviously correct" $
        True `shouldBe` True

    it "provides a message on expectation failure" $ do
      (annotate "obvious falsehood" $ True `shouldBe` False) `shouldThrow` expectationFailed (ExpectedButGot (Just "obvious falsehood") (show False) (show True))

    it "nests messages using newlines" $ do
      let msg0 = "obvious falsehood"
          msg1 = "welp"
          msgs = msg0 ++ "\n" ++ msg1
      (annotate msg0 $ annotate msg1 $ True `shouldBe` False) `shouldThrow` expectationFailed (ExpectedButGot (Just msgs) (show False) (show True))

    it "appends messages to Reason" $ do
      (annotate "welp" $ True `shouldSatisfy` (== False)) `shouldThrow` expectationFailed (Reason "welp\npredicate failed on: True")
