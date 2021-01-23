{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Expectations.FloatingSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Proxy
import           GHC.Fingerprint (fingerprint0)

import           Data.Bits.Floating

import           Test.Hspec.Expectations.Floating

infinity :: (Read a, RealFloat a) => a
infinity = read "Infinity"

nan :: (Read a, RealFloat a) => a
nan = read "NaN"

maxValue :: (Read a, RealFloat a, FloatingBits a b) => a
maxValue = nextDown infinity

times :: Integer -> (a -> a) -> a -> a
times n = foldr (.) id . genericReplicate n

spec :: Spec
spec = do
  describe "approximatelyEqual" $ do
    let
      compareWith :: Integer -> Float -> Float -> Bool
      compareWith = approximatelyEqual

      equals :: HasCallStack => Float -> Float -> Spec
      equals a b = do
        it ("returns True when comparing " ++ show a ++ " and " ++ show b) $ do
          approximatelyEqual 0 a b `shouldBe` True

      notEquals :: HasCallStack => Float -> Float -> Spec
      notEquals a b = do
        it ("returns False when comparing " ++ show a ++ " and " ++ show b) $ do
          approximatelyEqual 0 a b `shouldBe` False

    0 `equals` 0
    0 `equals` negate 0

    1 `equals` 1
    1 `notEquals` negate 1

    infinity `equals` infinity
    infinity `notEquals` negate infinity

    nan `equals` nan
    nan `equals` negate nan

    context "when ULP distance is within the specified threshold" $ do
      it "returns True" $ do
        property $ \ (NonNegative distance) n -> do
          compareWith distance (times distance nextUp n) n `shouldBe` True
          compareWith distance (times distance nextDown n) n `shouldBe` True

    context "when ULP distance is greater than the specified threshold" $ do
      it "returns False" $ do
        property $ \ (NonNegative distance) n -> do
          compareWith distance (times (succ distance) nextUp n) n `shouldBe` False
          compareWith distance (times (succ distance) nextDown n) n `shouldBe` False

  describe "ulpDistance" $ do
    context "with Float" $ do
      it "calculates the difference in discrete ULP steps" $ do
        property $ \ (NonNegative n) (a :: Float) -> do
          ulpDistance a (times n nextUp a) `shouldBe` n
          ulpDistance a (times n nextDown a) `shouldBe` n

    context "with Double" $ do
      it "calculates the difference in discrete ULP steps" $ do
        property $ \ (NonNegative n) (a :: Double) -> do
          ulpDistance a (times n nextUp a) `shouldBe` n
          ulpDistance a (times n nextDown a) `shouldBe` n

  describe "floatingToWord64" $ do
    context "with Float" $ do
      floatingToWord64WorksFor (Proxy :: Proxy Float)

    context "with Double" $ do
      floatingToWord64WorksFor (Proxy :: Proxy Double)

    context "with a datatype that is larger than Word64" $ do
      it "throws an exception" $ do
        evaluate (floatingToWord64 fingerprint0) `shouldThrow` errorCall "operand too large (8 < 16)"
  where
    floatingToWord64WorksFor proxy = do
      forM_ [0, 1, maxValue, infinity, nan] $ \ n -> do
        forM_ [n, negate n] $ \ m -> do
          it ("works for " ++ show m) $ do
            shouldWorkFor (m `asProxyTypeOf` proxy)
      it "works for arbitrary numbers" $ do
        property shouldWorkFor
      where
        shouldWorkFor n = floatingToWord64 n `shouldBe` referenceImplementation n
        referenceImplementation = fromIntegral . coerceToWord
