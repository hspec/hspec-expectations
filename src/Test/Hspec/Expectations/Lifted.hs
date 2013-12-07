-- |
-- Introductory documentation: <https://github.com/sol/hspec-expectations#readme>
module Test.Hspec.Expectations.Lifted (

-- * Setting expectations
  Expectation
, expectationFailure
, shouldBe
, shouldSatisfy
, shouldContain
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

import           Control.Exception
import           Control.Monad.IO.Class
import           Test.Hspec.Expectations hiding (expectationFailure, shouldBe, shouldSatisfy, shouldContain, shouldReturn, shouldThrow)
import qualified Test.Hspec.Expectations as E

infix 1 `shouldBe`, `shouldSatisfy`, `shouldContain`, `shouldReturn`, `shouldThrow`

liftIO2 :: MonadIO m => (a -> b -> IO r) -> a -> b -> m r
liftIO2 action a = liftIO . action a

-- | This is just an alias for HUnit's `assertFailure`.
expectationFailure :: MonadIO m => String -> m ()
expectationFailure = liftIO . E.expectationFailure

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@ (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe = liftIO2 E.shouldBe

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldSatisfy = liftIO2 E.shouldSatisfy

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in the second.
shouldContain :: (Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldContain = liftIO2 E.shouldContain

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (Show a, Eq a, MonadIO m) => IO a -> a -> m ()
shouldReturn = liftIO2 E.shouldReturn

-- |
-- @action \`shouldThrow\` selector@ sets the expectation that @action@ throws
-- an exception.  The precise nature of the expected exception is described
-- with a 'Selector'.
shouldThrow :: (Exception e, MonadIO m) => IO a -> Selector e -> m ()
shouldThrow = liftIO2 E.shouldThrow
