-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations (

-- * Setting expectations
  Expectation
, shouldBe
, shouldSatisfy
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

import           Prelude
import           Test.HUnit
import           Control.Exception
import           Data.Typeable

type Expectation = Assertion

infix 1 `shouldBe`, `shouldSatisfy`, `shouldReturn`, `shouldThrow`

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@ (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldBe` expected = actual @?= expected

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Expectation
v `shouldSatisfy` p = assertBool (show v ++ " did not satisfy predicate!") (p v)

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldReturn` expected = action >>= (`shouldBe` expected)

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
      assertFailure $
        "did not get expected exception: " ++ exceptionType
    Left e ->
      (`assertBool` p e) $
        "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: brocken Typeable instance"

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
