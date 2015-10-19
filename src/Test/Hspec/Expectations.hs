{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,1)
#define HAS_SOURCE_LOCATIONS
{-# LANGUAGE ImplicitParams #-}
#endif
-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations (

-- * Setting expectations
  Expectation
, expectationFailure
, shouldBe
, shouldSatisfy
, shouldStartWith
, shouldEndWith
, shouldContain
, shouldMatchList
, shouldReturn

, shouldNotBe
, shouldNotSatisfy
, shouldNotContain
, shouldNotReturn

, shouldBeOneOf
, shouldNotBeOneOf
, shouldReturnOneOf
, shouldNotReturnOneOf

-- * Expecting exceptions
, shouldThrow

-- ** Selecting exceptions
, Selector

-- ** Predefined type-based selectors
-- |
-- There are predefined selectors for some standard exceptions.  Each selector
-- is just @const True@ with an appropriate type.
, anyException
, anyErrorCall
, anyIOException
, anyArithException

-- ** Combinators for defining value-based selectors
-- |
-- Some exceptions (most prominently `ErrorCall`) have no `Eq` instance.
-- Selecting a specific value would require pattern matching.
--
-- For such exceptions, combinators that construct selectors are provided.
-- Each combinator corresponds to a constructor; it takes the same arguments,
-- and has the same name (but starting with a lower-case letter).
, errorCall
) where

import qualified Test.HUnit
import           Control.Exception
import           Data.Typeable
import           Data.List

import           Control.Monad (unless)

import           Test.Hspec.Expectations.Matcher

#ifdef HAS_SOURCE_LOCATIONS

import           GHC.Stack

#define with_loc(NAME, TYPE) NAME :: (?loc :: CallStack) => TYPE

#else

#define with_loc(NAME, TYPE) NAME :: TYPE

#endif

type Expectation = Test.HUnit.Assertion

with_loc(expectationFailure, String -> Expectation)
expectationFailure = Test.HUnit.assertFailure

with_loc(expectTrue, String -> Bool -> Expectation)
expectTrue msg b = unless b (expectationFailure msg)

infix 1 `shouldBe`, `shouldSatisfy`, `shouldStartWith`, `shouldEndWith`, `shouldContain`, `shouldMatchList`, `shouldReturn`, `shouldThrow`
infix 1 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@.
with_loc(shouldBe, (Show a, Eq a) => a -> a -> Expectation)
actual `shouldBe` expected = expectTrue ("expected: " ++ show expected ++ "\n but got: " ++ show actual) (actual == expected)

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
with_loc(shouldSatisfy, (Show a) => a -> (a -> Bool) -> Expectation)
v `shouldSatisfy` p = expectTrue ("predicate failed on: " ++ show v) (p v)

-- |
-- @actual \`shouldBeOneOf` es@ sets the expectation that @actual@ is one of
-- @es@
with_loc(shouldBeOneOf, (Show a, Eq a) => a -> [a] -> Expectation)
actual `shouldBeOneOf` expected = expectTrue ("expected one of:\n" ++ concatMap showe expected ++ " but got: " ++ show actual) (actual `elem` expected)
  where showe e = "  " ++ show e ++ "\n"

with_loc(compareWith, (Show a, Eq a) => (a -> a -> Bool) -> String -> a -> a -> Expectation)
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

-- |
-- @list \`shouldStartWith\` prefix@ sets the expectation that @list@ starts with @prefix@,
with_loc(shouldStartWith, (Show a, Eq a) => [a] -> [a] -> Expectation)
shouldStartWith = compareWith isPrefixOf "does not start with"

-- |
-- @list \`shouldEndWith\` suffix@ sets the expectation that @list@ ends with @suffix@,
with_loc(shouldEndWith, (Show a, Eq a) => [a] -> [a] -> Expectation)
shouldEndWith = compareWith isSuffixOf "does not end with"

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in @list@.
with_loc(shouldContain, (Show a, Eq a) => [a] -> [a] -> Expectation)
shouldContain = compareWith isInfixOf "does not contain"

-- |
-- @xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same
-- elements that @ys@ has, possibly in another order
with_loc(shouldMatchList, (Show a, Eq a) => [a] -> [a] -> Expectation)
xs `shouldMatchList` ys = maybe (return ()) expectationFailure (matchList xs ys)

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
with_loc(shouldReturn, (Show a, Eq a) => IO a -> a -> Expectation)
action `shouldReturn` expected = action >>= (`shouldBe` expected)

-- |
-- @action \`shouldReturnOneOf` expected@ sets the expectation that @action@
-- returns one of @expected@
with_loc(shouldReturnOneOf, (Show a, Eq a) => IO a -> [a] -> Expectation)
action `shouldReturnOneOf` expected = action >>= (`shouldBeOneOf` expected)

-- |
-- @actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not
-- equal to @notExpected@
with_loc(shouldNotBe, (Show a, Eq a) => a -> a -> Expectation)
actual `shouldNotBe` notExpected = expectTrue ("not expected: " ++ show actual) (actual /= notExpected)

-- |
-- @actual \`shouldNotBeOneOf` es@ sets the expectation that @actual@ is not one of
-- @es@
with_loc(shouldNotBeOneOf, (Show a, Eq a) => a -> [a] -> Expectation)
actual `shouldNotBeOneOf` expected = expectTrue ("not expected one of:\n" ++ concatMap showe expected ++ " but got: " ++ show actual) (actual `notElem` expected)
  where showe e = "  " ++ show e ++ "\n"

-- |
-- @v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
with_loc(shouldNotSatisfy, (Show a) => a -> (a -> Bool) -> Expectation)
v `shouldNotSatisfy` p = expectTrue ("predicate succeded on: " ++ show v) ((not . p) v)

-- |
-- @list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not
-- contained anywhere in @list@.
with_loc(shouldNotContain, (Show a, Eq a) => [a] -> [a] -> Expectation)
list `shouldNotContain` sublist = expectTrue errorMsg ((not . isInfixOf sublist) list)
  where
    errorMsg = show list ++ " does contain " ++ show sublist

-- |
-- @action \`shouldNotReturn\` notExpected@ sets the expectation that @action@
-- does not return @notExpected@.
with_loc(shouldNotReturn, (Show a, Eq a) => IO a -> a -> Expectation)
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)

-- |
-- @action \`shouldNotReturnOneOf` notExpected@ sets the expectation that @action@
-- does not return one of @notExpected@
with_loc(shouldNotReturnOneOf, (Show a, Eq a) => IO a -> [a] -> Expectation)
action `shouldNotReturnOneOf` notExpected = action >>= (`shouldNotBeOneOf` notExpected)

-- |
-- A @Selector@ is a predicate; it can simultaneously constrain the type and
-- value of an exception.
type Selector a = (a -> Bool)

-- |
-- @action \`shouldThrow\` selector@ sets the expectation that @action@ throws
-- an exception.  The precise nature of the expected exception is described
-- with a 'Selector'.
with_loc(shouldThrow, Exception e => IO a -> Selector e -> Expectation)
action `shouldThrow` p = do
  r <- try action
  case r of
    Right _ ->
      expectationFailure $
        "did not get expected exception: " ++ exceptionType
    Left e ->
      (`expectTrue` p e) $
        "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

anyException :: Selector SomeException
anyException = const True

anyErrorCall :: Selector ErrorCall
anyErrorCall = const True

errorCall :: String -> Selector ErrorCall
errorCall s (ErrorCall msg) = s == msg

anyIOException :: Selector IOException
anyIOException = const True

anyArithException :: Selector ArithException
anyArithException = const True
