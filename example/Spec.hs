module Spec (main, spec) where

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.HUnit.ShouldBe
import           Control.Exception

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "ShouldBe" $ do
    it "this should succeed" $ do
      "foo" `shouldBe` "foo"

    it "this should fail" $ do
      "foo" `shouldBe` "bar"


  describe "shouldThrow" $ do
    it "this should succeed" $ do
      throw DivideByZero `shouldThrow` (== DivideByZero)

    it "this should succeed" $ do
      error "foobar" `shouldThrow` anyException

    it "this should succeed" $ do
      error "foobar" `shouldThrow` anyErrorCall

    it "this should succeed" $ do
      error "foobar" `shouldThrow` errorCall "foobar"

    it "this should fail" $ do
      throw Overflow `shouldThrow` (== DivideByZero)

    it "this should fail" $ do
      return () `shouldThrow` anyException

    it "this should fail" $ do
      return () `shouldThrow` anyErrorCall

    it "this should fail" $ do
      error "foo" `shouldThrow` errorCall "foobar"
