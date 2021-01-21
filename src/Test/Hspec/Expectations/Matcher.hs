module Test.Hspec.Expectations.Matcher (matchList) where

import           Prelude hiding (showList)
import           Data.List

matchList :: (Show a, Eq a) => [a] -> [a] -> Maybe String
xs `matchList` ys
  | null extra && null missing = Nothing
  | otherwise = Just (err "")
  where
    extra   = xs \\ ys
    missing = ys \\ xs

    msgAndList msg zs = showString msg . showList zs . showString "\n"
    optMsgList msg zs = if null zs then id else msgAndList msg zs

    err :: ShowS
    err =
        showString "Actual list is not a permutation of expected list!\n"
      . msgAndList "  expected elements: " ys
      . msgAndList "  actual elements:   " xs
      . optMsgList "  missing elements:  " missing
      . optMsgList "  extra elements:    " extra

showList :: Show a => [a] -> ShowS
showList xs = showChar '[' . foldr (.) (showChar ']') (intersperse (showString ", ") $ map shows xs)
