Catchy combinators for HUnit
============================
(inspired by [ScalaTest's ShouldMatchers](http://www.scalatest.org/))

The three main primitives are `shouldBe`, `shouldSatisfy` and `shouldThrow`.
They can be used with [HUnit][hunit], or any
framework that integrates with HUnit, like
[test-framewor][test-framework] or
[Hspec][hspec].


An introductory example
-----------------------

Here is an example, that uses this in combination with Hspec.  It's a partial
specification of itself.

\begin{code}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit.ShouldBe
import Control.Exception

main :: IO ()
main = hspecX $ do

  describe "shouldBe" $ do

    it "asserts equality" $ do
      "foo" `shouldBe` "foo"

  describe "shouldSatisfy" $ do

    it "asserts that a predicate holds" $ do
      "bar" `shouldSatisfy` (not . null)

  describe "shouldThrow" $ do

    it "asserts that an exception is thrown" $ do
      throw DivideByZero `shouldThrow` (== DivideByZero)
\end{code}

shouldBe
--------

`shouldBe` is just an alias for HUnit's `@?=`.

shouldSatisfy
-------------

`shouldSatisfy` asserts that some predicate holds for a given value.

    "bar" `shouldSatisfy` (not . null)

It is similar to HUnit's `assertBool`, but gives a useful error message.

    >>> 23 `shouldSatisfy` (> 42)
    *** Exception: HUnitFailure "23 did not satisfy predicate!"

shouldThrow
-----------

`shouldThrow` asserts that an exception is thrown.  The precise nature of that
exception is described with a `Selector`.

    error "foobar" `shouldThrow` anyException


A `Selector` is a predicate, it can simultaneously constrain the type and value
of an exception.

    throw DivideByZero `shouldThrow` (== DivideByZero)


To select all exceptions of a given type, `const True` can be used.

    error "foobar" `shouldThrow` (const True :: Selector ErrorCall)


For convenience, predefined selectors for some standard exceptions are
provided.

    error "foobar" `shouldThrow` anyErrorCall


Some exceptions (like `ErrorCall`) have no `Eq` instance, so checking for a
specific value requires pattern matching.

    error "foobar" `shouldThrow` (\e -> case e of
        ErrorCall "foobar" -> True
        _ -> False
        )


For such exceptions, combinators that construct selectors are provided. Each
combinator corresponds to a constructor; it takes the same arguments, and has
the same name (but starting with a lower-case letter).

    error "foobar" `shouldThrow` errorCall "foobar"

[hunit]: http://hackage.haskell.org/package/HUnit
[test-framework]: http://hackage.haskell.org/package/test-framework
[hspec]: http://hackage.haskell.org/package/hspec
