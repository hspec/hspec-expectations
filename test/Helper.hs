module Helper where

import           Test.Hspec.HUnit()
import           Test.Hspec (it)
import           Test.Hspec.Runner
import           Test.HUnit
import           System.IO.Silently

shouldResultIn :: Assertion -> String -> IO ()
shouldResultIn expectation result = do
  r <- fmap (last . lines) . capture_ . hspecWith defaultConfig $ do
    it "" expectation
  r @?= result

shouldHold :: Assertion -> Assertion
shouldHold = (`shouldResultIn` "1 example, 0 failures")

shouldNotHold :: Assertion -> Assertion
shouldNotHold = (`shouldResultIn` "1 example, 1 failure")
