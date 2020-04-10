{-# LANGUAGE CPP #-}
-- |
-- Experimental combinators, that may become part of the main distribution, if
-- they turn out to be useful for a wider audience.
module Test.Hspec.Expectations.Contrib (
-- * Predicates
-- | (useful in combination with `shouldSatisfy`)
  isLeft
, isRight

-- * Annotating expectations
, annotate
) where

import Control.Exception
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(..))

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
-- If you have a test case that has multiple assertions, you can use the
-- 'annotate' function to provide a string message that will be attached to
-- the 'Expectation'.
--
-- @
-- describe "annotate" $ do
--   it "adds the message" $ do
--     annotate "obvious falsehood" $ do
--       True `shouldBe` False
--
-- ========>
--
-- 1) annotate, adds the message
--       obvious falsehood
--       expected: False
--        but got: True
-- @
--
-- @since 0.8.3
annotate :: String -> IO a -> IO a
annotate message = handle $ \ (HUnitFailure loc reason) -> throwIO . HUnitFailure loc $ case reason of
  Reason err -> Reason $ addMessage err
  ExpectedButGot err expected got -> ExpectedButGot (Just $ maybe message addMessage err) expected got
  where
    addMessage err
      | null err = message
      | otherwise = message ++ "\n" ++ err
