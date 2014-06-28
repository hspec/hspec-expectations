-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations (

-- * Setting expectations
  Expectation
, expectationFailure
, shouldBe
, shouldNotBe
, shouldSatisfy
, shouldNotSatisfy
, shouldContain
, shouldNotContain
, shouldMatchList
, shouldReturn

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

import           Test.HUnit (Assertion, (@?=), assertBool, assertFailure)
import           Control.Exception
import           Data.Typeable
import           Data.List ((\\), isInfixOf)

type Expectation = Assertion

-- | This is just an alias for HUnit's `assertFailure`.
expectationFailure :: String -> Expectation
expectationFailure = assertFailure

infix 1 `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldNotSatisfy`, `shouldContain`, `shouldNotContain`, `shouldMatchList`, `shouldReturn`, `shouldNotReturn`, `shouldThrow`

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@ (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldBe` expected = actual @?= expected

-- |
-- @actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not
-- equal to @notExpected@
shouldNotBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldNotBe` notExpected = assertBool ("not expected: " ++ show actual) (actual /= notExpected)

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Expectation
v `shouldSatisfy` p = assertBool ("predicate failed on: " ++ show v) (p v)

-- |
-- @v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (Show a) => a -> (a -> Bool) -> Expectation
v `shouldNotSatisfy` p = assertBool ("predicate succeded on: " ++ show v) ((not . p) v)

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in the second.
shouldContain :: (Show a, Eq a) => [a] -> [a] -> Expectation
list `shouldContain` sublist = assertBool errorMsg (sublist `isInfixOf` list)
  where
    errorMsg = show list ++ " doesn't contain " ++ show sublist
    
-- |
-- @list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not
-- contained anywhere in the second.
shouldNotContain :: (Show a, Eq a) => [a] -> [a] -> Expectation
list `shouldNotContain` sublist = assertBool errorMsg ((not . isInfixOf) sublist list)
  where
    errorMsg = show list ++ " does contain " ++ show sublist

-- |
-- @xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same
-- elements that @ys@ has, possibly in another order
shouldMatchList :: (Show a, Eq a) => [a] -> [a] -> Expectation
xs `shouldMatchList` ys = assertBool errorMsg (all null [xs \\ ys, ys \\ xs])
  where
    errorMsg = show ys ++ " is not a permutation of " ++ show xs

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldReturn` expected = action >>= (`shouldBe` expected)

-- |
-- @action \`shouldNotReturn\` notExpected@ sets the expectation that @action@
-- does not return @expected@.
shouldNotReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldNotReturn` notExpected = action >>= (`shouldNotBe` notExpected)

-- |
-- A @Selector@ is a predicate; it can simultaneously constrain the type and
-- value of an exception.
type Selector a = (a -> Bool)

-- |
-- @action \`shouldThrow\` selector@ sets the expectation that @action@ throws
-- an exception.  The precise nature of the expected exception is described
-- with a 'Selector'.
shouldThrow :: Exception e => IO a -> Selector e -> Expectation
action `shouldThrow` p = do
  r <- try action
  case r of
    Right _ ->
      expectationFailure $
        "did not get expected exception: " ++ exceptionType
    Left e ->
      (`assertBool` p e) $
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
