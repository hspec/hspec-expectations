module Main where

import Test.Hspec

import qualified Test.Hspec.ExpectationsSpec
import qualified Test.Hspec.Expectations.MatcherSpec
import qualified Test.Hspec.Expectations.ContribSpec

spec :: Spec
spec = do
  describe "Test.Hspec.ExpectationsSpec" Test.Hspec.ExpectationsSpec.spec
  describe "Test.Hspec.Expectations.MatcherSpec" Test.Hspec.Expectations.MatcherSpec.spec
  describe "Test.Hspec.Expectations.ContribSpec" Test.Hspec.Expectations.ContribSpec.spec

main :: IO ()
main = hspec spec
