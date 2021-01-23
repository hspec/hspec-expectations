module Test.Hspec.Expectations.Floating (
  Storable
, approximatelyEqual
, ulpDistance
, floatingToWord64
) where

import           Data.Word
import           Foreign
import           System.IO.Unsafe (unsafePerformIO)

import           Data.CallStack

approximatelyEqual :: (RealFloat a, Storable a) => Integer -> a -> a -> Bool
approximatelyEqual maxUlpDistance a b
  | isNaN a = isNaN b
  | a == b = True
  | otherwise = ulpDistance a b <= maxUlpDistance

ulpDistance :: (RealFloat a, Storable a) => a -> a -> Integer
ulpDistance a b = abs (toWord a - toWord b)
  where
    toWord = fromIntegral . floatingToWord64

floatingToWord64 :: HasCallStack => Storable a => a -> Word64
floatingToWord64 a
  | sizeOf_b < sizeOf_a = error $ concat ["operand too large (", show sizeOf_b, " < ", show sizeOf_a, ")"]
  | (alignment_b `mod` alignment_a) /= 0 = error $ concat ["alignment mismatch (", show alignment_a, " is not a factor of ", show alignment_b, ")"]
  | otherwise = b
  where
    alignment_a = alignment a
    alignment_b = alignment b
    sizeOf_a = sizeOf a
    sizeOf_b = sizeOf b
    b = unsafePerformIO $ do
      allocaBytesAligned sizeOf_b sizeOf_b $ \ ptr -> do
        poke ptr 0
        poke (castPtr ptr) a
        peek ptr
