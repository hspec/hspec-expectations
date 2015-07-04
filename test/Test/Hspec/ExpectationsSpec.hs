{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.ExpectationsSpec (spec) where

import           Control.Exception
import           Test.Hspec (Spec, describe, it)
import           Test.HUnit.Lang

import           Test.Hspec.Expectations

deriving instance Eq HUnitFailure

spec :: Spec
spec = do
  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      "foo" `shouldBe` "foo"

    it "fails if arguments are not equal" $ do
      ("foo" `shouldBe` "bar") `shouldThrow` (== HUnitFailure "expected: \"bar\"\n but got: \"foo\"")

  describe "shouldSatisfy" $ do
    it "succeeds if value satisfies predicate" $ do
      "" `shouldSatisfy` null

    it "fails if value does not satisfy predicate" $ do
      ("foo" `shouldSatisfy` null) `shouldThrow` (== HUnitFailure "predicate failed on: \"foo\"")

  describe "shouldReturn" $ do
    it "succeeds if arguments represent equal values" $ do
      return "foo" `shouldReturn` "foo"

    it "fails if arguments do not represent equal values" $ do
      (return "foo" `shouldReturn` "bar") `shouldThrow` (== HUnitFailure "expected: \"bar\"\n but got: \"foo\"")

  describe "shouldStartWith" $ do
    it "succeeds if second is prefix of first" $ do
      "hello world" `shouldStartWith` "hello"

    it "fails if second is not prefix of first" $ do
      ("hello world" `shouldStartWith` "world") `shouldThrow` (== HUnitFailure "\"hello world\" does not start with \"world\"")

  describe "shouldEndWith" $ do
    it "succeeds if second is suffix of first" $ do
      "hello world" `shouldEndWith` "world"

    it "fails if second is not suffix of first" $ do
      ("hello world" `shouldEndWith` "hello") `shouldThrow` (== HUnitFailure "\"hello world\" does not end with \"hello\"")

  describe "shouldContain" $ do
    it "succeeds if second argument is contained in the first" $ do
      "I'm an hello world message" `shouldContain` "an hello"

    it "fails if first argument does not contain the second" $ do
      ("foo" `shouldContain` "bar") `shouldThrow` (== HUnitFailure "\"foo\" does not contain \"bar\"")

  describe "shouldThrow" $ do
    it "can be used to require a specific exception" $ do
      throwIO DivideByZero `shouldThrow` (== DivideByZero)

    it "can be used to require any exception" $ do
      error "foobar" `shouldThrow` anyException

    it "can be used to require an exception of a specific type" $ do
      error "foobar" `shouldThrow` anyErrorCall

    it "can be used to require a specific exception" $ do
      error "foobar" `shouldThrow` errorCall "foobar"

    it "fails, if a required specific exception is not thrown" $ do
      (throwIO Overflow `shouldThrow` (== DivideByZero)) `shouldThrow` (== HUnitFailure "predicate failed on expected exception: ArithException (arithmetic overflow)")

    it "fails, if any exception is required, but no exception occurs" $ do
      (return () `shouldThrow` anyException) `shouldThrow` (== HUnitFailure "did not get expected exception: SomeException")

    it "fails, if a required exception of a specific type is not thrown" $ do
      (return () `shouldThrow` anyErrorCall) `shouldThrow` (== HUnitFailure "did not get expected exception: ErrorCall")

    it "fails, if a required specific exception is not thrown" $ do
      (error "foo" `shouldThrow` errorCall "foobar") `shouldThrow` (== HUnitFailure "predicate failed on expected exception: ErrorCall (foo)")
