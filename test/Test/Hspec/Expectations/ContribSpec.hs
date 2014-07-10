module Test.Hspec.Expectations.ContribSpec (spec) where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Utils

spec :: Spec
spec = do
  describe "shouldNotBe" $ do
    it "succeeds if arguments are not equal" $ do
      shouldHold $
        "foo" `shouldNotBe` "bar"
    it "fails if arguments are equal" $ do
      shouldNotHold $
        "foo" `shouldNotBe` "foo" 
        
  describe "shouldNotSatisfy" $ do
    it "succeeds if value does not satisfy predicate" $ do
      shouldHold $
        "bar" `shouldNotSatisfy` null
    it "fails if the value does satisfy predicate" $ do
      shouldNotHold $
        "" `shouldNotSatisfy` null

  describe "shouldNotReturn" $ do
    it "succeeds if arguments does not represent equal values" $ do
      shouldHold $
        return "foo" `shouldNotReturn` "bar"

    it "fails if arguments do represent equal values" $ do
      shouldNotHold $
        return "foo" `shouldNotReturn` "foo"

  describe "shouldNotContain" $ do
    it "succeeds if second argument is not contained in the first" $ do
      shouldHold $
        "I'm an hello world message" `shouldNotContain` "test"

    it "succeds not only with strings" $ do
      shouldHold $
        ([1,2,3,4,5] :: [Int]) `shouldNotContain` ([9, 6] :: [Int])

    it "fails if first argument does contain the second" $ do
      shouldNotHold $
        "foo abc def" `shouldNotContain` "def"
