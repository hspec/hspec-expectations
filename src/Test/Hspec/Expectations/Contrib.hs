-- |
-- Experimental combinators, that may become part of the main distribution, if
-- they turn out to be useful for a wider audience.
module Test.Hspec.Expectations.Contrib (

  module Test.Hspec.Expectations

-- * Predicates
-- | (useful in combination with `shouldSatisfy`)
, isLeft
, isRight
, shouldNotBe
, shouldNotSatisfy
, shouldNotReturn
, shouldNotContain
) where

import Test.Hspec.Expectations
import Test.HUnit (assertBool)
import Data.List (isInfixOf)

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

infix 1 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

-- |
-- @actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not
-- equal to @notExpected@
shouldNotBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldNotBe` notExpected = assertBool ("not expected: " ++ show actual) (actual /= notExpected)

-- |
-- @v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (Show a) => a -> (a -> Bool) -> Expectation
v `shouldNotSatisfy` p = assertBool ("predicate succeded on: " ++ show v) ((not . p) v)

-- |
-- @list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not
-- contained anywhere in the second.
shouldNotContain :: (Show a, Eq a) => [a] -> [a] -> Expectation
list `shouldNotContain` sublist = assertBool errorMsg ((not . isInfixOf sublist) list)
  where
    errorMsg = show list ++ " does contain " ++ show sublist

-- |
-- @action \`shouldNotReturn\` notExpected@ sets the expectation that @action@
-- does not return @expected@.
shouldNotReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)
