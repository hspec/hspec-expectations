{-# LANGUAGE CPP #-}
-- |
-- Experimental combinators, that may become part of the main distribution, if
-- they turn out to be useful for a wider audience.
module Test.Hspec.Expectations.Contrib (
-- * Predicates
-- | (useful in combination with `shouldSatisfy`)
  isLeft
, isRight
, shouldInclude
, shouldIncludeAll
) where

import           Control.Monad (unless)
import           Test.Hspec.Expectations (HasCallStack, Expectation, expectationFailure)
import           Data.Foldable (foldl')
import           Data.List (intercalate)

#if MIN_VERSION_base(4,7,0)
import Data.Either
#else

isLeft :: Either a b -> Bool
{-# DEPRECATED isLeft "use Data.Either.Compat.isLeft from package base-compat instead" #-}
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
{-# DEPRECATED isRight "use Data.Either.Compat.isRight from package base-compat instead" #-}
isRight (Left  _) = False
isRight (Right _) = True
#endif


-- |
-- @container \`shouldInclude\` item@ sets the expectation that @item@ appears at least once
-- in @container@.
shouldInclude :: (HasCallStack, Show a, Show (t a), Eq a, Foldable t)
              => t a
              -> a
              -> Expectation
shouldInclude = compareWithAny elem "does not include"


-- |
-- @container \`shouldIncludeAll\` subContainer@ sets the expectation
-- that all items in @subContainer@ appear at least once in @container@.
shouldIncludeAll :: (HasCallStack, Foldable t1, Foldable t2, Show a, Show (t1 a), Show (t2 a), Eq a)
                 => t1 a
                 -> t2 a
                 -> Expectation
actual `shouldIncludeAll` subset = expectTrue message (all isIncluded subset)
  where
    isIncluded = (`elem` actual)
    message = show actual <> " did not include all of " <> show subset <> " - missing: " <> missing
    missing = intercalate ", " (fmap show missingItems)
    missingItems = foldl' accumulateIfIncluded [] subset
    accumulateIfIncluded acc val = if isIncluded val then acc else (val : acc)


-- Cloned from 'Test.Hspec.Expectations'
expectTrue :: HasCallStack
           => String
           -> Bool
           -> Expectation
expectTrue msg b = unless b (expectationFailure msg)


compareWithAny :: (HasCallStack, Show a, Show b)
               => (a -> b -> Bool)
               -> String
               -> b
               -> a
               -> Expectation
compareWithAny comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
