-- |
-- Experimental combinators, that may become part of the main distribution, if
-- they turn out to be useful for a wider audience.
module Test.HUnit.ShouldBe.Contrib (

-- * Predicates
-- | (useful in combination with `shouldSatisfy`)
  isLeft
, isRight
) where

import Test.HUnit.ShouldBe

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
