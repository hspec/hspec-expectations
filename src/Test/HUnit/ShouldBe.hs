module Test.HUnit.ShouldBe (
  shouldBe
, shouldSatisfy
, shouldThrow
, Selector
, anyException
, anyErrorCall
, errorCall
, anyIOException
, anyArithException
) where

import           Prelude hiding (catch)
import           Test.HUnit
import           Control.Exception
import           Data.Typeable
import           Control.Arrow ((&&&))

infix 1 `shouldBe`, `shouldThrow`

-- |
-- @actual \`shouldBe\` expected@ asserts that @actual@ is equal to @expected@
-- (this is just an alias for `@?=`).
shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected

shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Assertion
v `shouldSatisfy` p = assertBool (show v ++ " did not satisfy predicate!") (p v)


type Selector a = (a -> Bool)

shouldThrow :: Exception e => IO a -> (e -> Bool) -> Assertion
action `shouldThrow` p = do
  m <- (action >> return Nothing) `catch` (return . Just . (p &&& id))
  case m of
    Nothing ->
      assertFailure msgNothing
    Just (r, e) ->
      assertBool (msgFailure e) r
  where
    msgNothing = "did not get expected exception: "
            ++ (show . typeOf . instanceOf $ p)

    msgFailure e = "predicate failed on expected exception: "
            ++ (show . typeOf $ e) ++ " (" ++ show e ++ ")"

    instanceOf :: Selector a -> a
    instanceOf _ = error "Test.HUnit.ShouldBe.shouldThrow: brocken Typeable instance"

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
