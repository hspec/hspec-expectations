module Test.Hspec.Expectations.ContribSpec (spec) where

import           Test.Hspec
import           Test.HUnit.Lang

import           Test.Hspec.Expectations.Contrib
import           Test.Hspec.ExpectationsSpec ()

spec :: Spec
spec = do
  describe "shouldNotBe" $ do
    it "succeeds if arguments are not equal" $ do
      "foo" `shouldNotBe` "bar"

    it "fails if arguments are equal" $ do
      ("foo" `shouldNotBe` "foo") `shouldThrow` (== HUnitFailure "not expected: \"foo\"")

  describe "shouldNotSatisfy" $ do
    it "succeeds if value does not satisfy predicate" $ do
      "bar" `shouldNotSatisfy` null

    it "fails if the value does satisfy predicate" $ do
      ("" `shouldNotSatisfy` null) `shouldThrow` (== HUnitFailure "predicate succeded on: \"\"")

  describe "shouldNotReturn" $ do
    it "succeeds if arguments does not represent equal values" $ do
      return "foo" `shouldNotReturn` "bar"

    it "fails if arguments do represent equal values" $ do
      (return "foo" `shouldNotReturn` "foo") `shouldThrow` (== HUnitFailure "not expected: \"foo\"")

  describe "shouldNotContain" $ do
    it "succeeds if second argument is not contained in the first" $ do
      "I'm an hello world message" `shouldNotContain` "test"

    it "fails if first argument does contain the second" $ do
      ("foo abc def" `shouldNotContain` "def") `shouldThrow` (== HUnitFailure "\"foo abc def\" does contain \"def\"")
