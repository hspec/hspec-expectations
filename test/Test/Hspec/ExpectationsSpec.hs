{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,8,1)
#define HAS_SOURCE_LOCATIONS
{-# LANGUAGE ImplicitParams #-}
#endif
module Test.Hspec.ExpectationsSpec (spec) where

import           Control.Exception
import           Test.HUnit.Lang
import           Test.Hspec (Spec, describe, it)

import           Test.Hspec.Expectations

#ifdef HAS_SOURCE_LOCATIONS

#if !(MIN_VERSION_base(4,9,0))
import           GHC.SrcLoc
#endif

import           GHC.Stack

expectationFailed :: (?loc :: CallStack) => String -> HUnitFailure -> Bool
expectationFailed msg (HUnitFailure l m) = m == msg && (setColumn <$> l) == location
  where
    location :: Maybe Location
    location = case reverse (getCallStack ?loc) of
      (_, loc) : _ -> Just $ Location (srcLocFile loc) (srcLocStartLine loc) 0
      _ -> Nothing

    setColumn :: Location -> Location
    setColumn loc_ = loc_{locationColumn = 0}
#else
expectationFailed :: String -> HUnitFailure -> Bool
expectationFailed msg e = e == HUnitFailure Nothing msg
#endif

spec :: Spec
spec = do
  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      "foo" `shouldBe` "foo"

    it "fails if arguments are not equal" $ do
      ("foo" `shouldBe` "bar") `shouldThrow` expectationFailed "expected: \"bar\"\n but got: \"foo\""

  describe "shouldBeNear" $ do
    it "succeeds if arguments are equal" $ do
      1.23456789 `shouldBeNear` (1.23456789 :: Float)

    it "fails if arguments are not equal" $ do
      (1.0 `shouldBe` (2.0 :: Float)) `shouldThrow` expectationFailed "expected: 2.0\n but got: 1.0"

    it "succeeds if one argument is zero and the other less than epsilon" $ do
      0.0 `shouldBeNear` (-1e-16 :: Float)

    it "succeeds for large values near one another" $ do
      1e20 `shouldBeNear` (1e20 + 1 :: Float)

    it "succeeds for a small number near zero" $ do
      1e-300 `shouldBeNear` (0.0 :: Double)

#if __GLASGOW_HASKELL__ != 710
    -- For some reason this fails with ghc-7.10.2 and 7.10.3.
    it "fails for large values not near each other" $ do
      (1.1e20 `shouldBeNear` (2.1e20 :: Double)) `shouldThrow` expectationFailed "expected: 2.1e20\n but got: 1.1e20"

    it "fails for two small numbers that are not near each other" $ do
      (1.1e-300 `shouldBeNear` (1e-300 :: Double)) `shouldThrow` expectationFailed "expected: 1.0e-300\n but got: 1.1e-300"
#endif

  describe "shouldSatisfy" $ do
    it "succeeds if value satisfies predicate" $ do
      "" `shouldSatisfy` null

    it "fails if value does not satisfy predicate" $ do
      ("foo" `shouldSatisfy` null) `shouldThrow` expectationFailed "predicate failed on: \"foo\""

  describe "shouldReturn" $ do
    it "succeeds if arguments represent equal values" $ do
      return "foo" `shouldReturn` "foo"

    it "fails if arguments do not represent equal values" $ do
      (return "foo" `shouldReturn` "bar") `shouldThrow` expectationFailed "expected: \"bar\"\n but got: \"foo\""

  describe "shouldStartWith" $ do
    it "succeeds if second is prefix of first" $ do
      "hello world" `shouldStartWith` "hello"

    it "fails if second is not prefix of first" $ do
      ("hello world" `shouldStartWith` "world") `shouldThrow` expectationFailed "\"hello world\" does not start with \"world\""

  describe "shouldEndWith" $ do
    it "succeeds if second is suffix of first" $ do
      "hello world" `shouldEndWith` "world"

    it "fails if second is not suffix of first" $ do
      ("hello world" `shouldEndWith` "hello") `shouldThrow` expectationFailed "\"hello world\" does not end with \"hello\""

  describe "shouldContain" $ do
    it "succeeds if second argument is contained in the first" $ do
      "I'm an hello world message" `shouldContain` "an hello"

    it "fails if first argument does not contain the second" $ do
      ("foo" `shouldContain` "bar") `shouldThrow` expectationFailed "\"foo\" does not contain \"bar\""

  describe "shouldNotBe" $ do
    it "succeeds if arguments are not equal" $ do
      "foo" `shouldNotBe` "bar"

    it "fails if arguments are equal" $ do
      ("foo" `shouldNotBe` "foo") `shouldThrow` expectationFailed "not expected: \"foo\""

  describe "shouldNotSatisfy" $ do
    it "succeeds if value does not satisfy predicate" $ do
      "bar" `shouldNotSatisfy` null

    it "fails if the value does satisfy predicate" $ do
      ("" `shouldNotSatisfy` null) `shouldThrow` expectationFailed "predicate succeeded on: \"\""

  describe "shouldNotReturn" $ do
    it "succeeds if arguments does not represent equal values" $ do
      return "foo" `shouldNotReturn` "bar"

    it "fails if arguments do represent equal values" $ do
      (return "foo" `shouldNotReturn` "foo") `shouldThrow` expectationFailed "not expected: \"foo\""

  describe "shouldNotContain" $ do
    it "succeeds if second argument is not contained in the first" $ do
      "I'm an hello world message" `shouldNotContain` "test"

    it "fails if first argument does contain the second" $ do
      ("foo abc def" `shouldNotContain` "def") `shouldThrow` expectationFailed "\"foo abc def\" does contain \"def\""

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
      (throwIO Overflow `shouldThrow` (== DivideByZero)) `shouldThrow` expectationFailed "predicate failed on expected exception: ArithException (arithmetic overflow)"

    it "fails, if any exception is required, but no exception is thrown" $ do
      (return () `shouldThrow` anyException) `shouldThrow` expectationFailed "did not get expected exception: SomeException"

    it "fails, if an exception of a specific type is required, but no exception is thrown" $ do
      (return () `shouldThrow` anyErrorCall) `shouldThrow` expectationFailed "did not get expected exception: ErrorCall"
