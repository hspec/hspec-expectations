# Catchy combinators for HUnit

(inspired by [ScalaTest's ShouldMatchers](http://www.scalatest.org/))

The three main primitives are `shouldBe`, `shouldSatisfy` and
`shouldThrow`. They can be used with
[HUnit](http://hackage.haskell.org/package/HUnit), or any framework that
integrates with HUnit, like
[test-framework](http://hackage.haskell.org/package/test-framework) or
[Hspec](http://hackage.haskell.org/package/hspec).

## An introductory example

Here is an example that uses Hspec. It's a partial specification of
itself.

~~~ {.haskell .literate}
import Test.Hspec
import Control.Exception

main :: IO ()
main = hspec $ do

  describe "shouldBe" $ do

    it "asserts equality" $ do
      "foo" `shouldBe` "foo"

  describe "shouldSatisfy" $ do

    it "asserts that a predicate holds" $ do
      "bar" `shouldSatisfy` (not . null)

  describe "shouldThrow" $ do

    it "asserts that an exception is thrown" $ do
      throw DivideByZero `shouldThrow` (== DivideByZero)
~~~

## shouldBe

`shouldBe` is just an alias for HUnit's `@?=`.

## shouldSatisfy

`shouldSatisfy` asserts that some predicate holds for a given value.

~~~ {.haskell}
"bar" `shouldSatisfy` (not . null)
~~~

It is similar to HUnit's `assertBool`, but gives a useful error message.

    >>> 23 `shouldSatisfy` (> 42)
    *** Exception: HUnitFailure "23 did not satisfy predicate!"

## shouldReturn

`shouldReturn` asserts that an action returns a given value.

~~~ {.haskell}
return "bar" `shouldReturn` "bar"
~~~

## shouldThrow

`shouldThrow` asserts that an exception is thrown. The precise nature of
that exception is described with a `Selector`.

~~~ {.haskell}
error "foobar" `shouldThrow` anyException
~~~

A `Selector` is a predicate, it can simultaneously constrain the type
and value of an exception.

~~~ {.haskell}
throw DivideByZero `shouldThrow` (== DivideByZero)
~~~

To select all exceptions of a given type, `const True` can be used.

~~~ {.haskell}
error "foobar" `shouldThrow` (const True :: Selector ErrorCall)
~~~

For convenience, predefined selectors for some standard exceptions are
provided.

~~~ {.haskell}
error "foobar" `shouldThrow` anyErrorCall
~~~

Some exceptions (like `ErrorCall`) have no `Eq` instance, so checking
for a specific value requires pattern matching.

~~~ {.haskell}
error "foobar" `shouldThrow` (\e -> case e of
    ErrorCall "foobar" -> True
    _ -> False
    )
~~~

For such exceptions, combinators that construct selectors are provided.
Each combinator corresponds to a constructor; it takes the same
arguments, and has the same name (but starting with a lower-case
letter).

~~~ {.haskell}
error "foobar" `shouldThrow` errorCall "foobar"
~~~
