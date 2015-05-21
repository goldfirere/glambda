A Practical Introduction to Haskell GADTs
=========================================

This repo and these notes are for a talk given at [LambdaConf][1] in
Boulder, CO, USA, on 22 May, 2015.

See below for more information about `glambda` itself.

Setup
-----

**Do this first!**

We will be working from my [glambda][2] project to learn about Generalized
Algebraic Datatypes (GADTs). The `glambda` package has a non-trivial set
of dependencies. While I'm introducing myself and GADTs, generally, it would
be wise to download and compile all of the dependencies onto your laptop.
Then, when we get to hands-on exercises, you'll be all ready to go.

Here's what to do

    > git clone git://github.com/goldfirere/glambda.git
    > cd glambda
    > cabal sandbox init
    > cabal install --only-dependencies --enable-tests -j<# of CPUs you have>

This should make your computer spin for a little while. In the meantime,
enjoy the talk! (If you have trouble with `cabal sandbox`, possibly because
of an old `cabal`, try the sequence of commands above without that step.)

Exercises 1 and 2 do *not* require those dependencies, so you can work on
them while compiling dependencies.

Instructions for exercises:
 - [Exercise 1](Exercise1.md)
 - [Exercise 2](Exercise2.md)
 - [Exercise 3](Exercise3.md)

[1]: http://www.degoesconsulting.com/lambdaconf-2015/
[2]: https://github.com/goldfirere/glambda

More information about `glambda` appears below:

The Glamorous Glambda Interpreter
=================================

Glambda is a simply-typed lambda calculus interpreter. While it is intended
to be easy-to-use and help users learn about the lambda calculus, its real
strength is its implementation, which makes heavy use of GADTs, and is designed
to serve as a showcase of writing a real-world program with extra compile-time
guarantees.

This manual focuses only on the user experience. The structure of the code
will be described in a series of GADT programming tutorials coming out
Real Soon Now.

Example session
---------------

Saying `cabal install glambda` will produce an executable `glam`. `glam` is
the lambda-calculus interpreter. It is GHCi-like, accepting commands beginning
with a `:`. Here is an example session:

~~~
                   \\\\\\
                    \\\\\\
                 /-\ \\\\\\
                |   | \\\\\\
                 \-/|  \\\\\\
                    | //\\\\\\
                 \-/ ////\\\\\\
                    //////\\\\\\
                   //////  \\\\\\
                  //////    \\\\\\
Welcome to the Glamorous Glambda interpreter, version 1.0.
λ> (\x:Int.x + 2) 5
7 : Int
λ> revapp = \x:Int.\y:Int->Int.y x
revapp = λ#. λ#. #0 #1 : Int -> (Int -> Int) -> Int
λ> not = \b:Bool.if b then false else true
not = λ#. if #0 then false else true : Bool -> Bool
λ> revapp (3 < 4) not
Bad function application.
  Function type: Int -> (Int -> Int) -> Int
  Argument type: Bool
in the expression 'revapp (3 < 4)'
λ> not (3 < 4)
false : Bool
λ> :type revapp (10 % 3)
(λ#. λ#. #0 #1) (10 % 3) : (Int -> Int) -> Int
λ> :step revapp (10 % 3) (\x:Int.x * 2)
(λ#. λ#. #0 #1) (10 % 3) (λ#. #0 * 2) : Int
--> (λ#. #0 (10 % 3)) (λ#. #0 * 2) : Int
--> (λ#. #0 * 2) (10 % 3) : Int
--> 10 % 3 * 2 : Int
--> 1 * 2 : Int
--> 2 : Int
2 : Int
λ> :quit
Good-bye.
~~~

As you can see, glambda uses [de Bruijn indices][1] to track variable binding.
In the actual output (if your console supports it), the binders (`#`) and
usage sites (`#0`, `#1`) are colored so that humans can easily tell which
variable is used where.

[1]: https://en.wikipedia.org/wiki/De_Bruijn_index

You can also see above that the input to glambda must be fully annotated;
glambda does *not* do type inference. However, note that types on binders
do not appear in the output: once an input is type-checked, the type information
is erased. Yet, because of the use of GADTs in the implementation, we
can be sure that the reductions are type-safe.

The Language
------------

The glambda language is an explicitly typed simply typed lambda calculus,
with integers (`Int`) and booleans (`Bool`). The following operators are
supported, with their usual meanings, associativity, and precedence:

    + - * / % < <= > >= ==

The only slightly unusual member of this list is `%`, which takes a modulus,
like in C-inspired languages. The division operator `/` does integer division,
naturally.

Glambda supports a ternary conditional operator, demonstrated in the
snippet above, as `if <boolean expression> then <exp> else <exp>`.

Integer constants must be positive. Subtract from 0 to get a negative integer.

Boolean constants are spelled `false` and `true`.

Comments are exactly as in Haskell: `--` starts a line comment, and
`{- ... -}` is a block comment. Comments can be nested.

Variable names are as in Haskell: names must start with a letter or
underscore (although case is immaterial) and then may have letters, numbers,
and underscores.

The language is not whitespace-aware.

Most of what we have seen are *expressions*. Glambda also supports *statements*.
A statement is either an expression or has the form `<name> = <expression>`.
This latter form assigns a global variable to the expression. These global
variables are expanded during type-checking: they are more like macros than
proper cells holding information. Statements can be separated by `;`.

The Interface
-------------

When you type an expression into the glambda interpreter, it is evaluated
fully, and the value is printed, along with its type.

When you type a global variable assignment, that variable is assigned, and
its (unevaluated) contents are printed, along with its type.

You can also run commands, as described below. Commands all start with a
leading `:`, and that `:` must be the first character on the input line.
Arguments to a command are given after the command itself. Commands can
be abbreviated by typing an unambiguous prefix to a command. For example,
`:t` can be used to get an expression's type, because no other command
begins with `t`.

Commands
--------

`:quit` quits the glambda interpreter.

`:lex` lexes the given text and pretty-prints the result.

`:parse` parses the given text and pretty-prints the result.

`:eval` type-checks and evaluates the given expression. This is
the default behavior at the command line.

`:step` runs the given expression through the single-step semantics.
This shows you every step of the way from your expression down to
a value. This uses a *different* evaluation strategy than `:eval` does,
but the result should always be the same.

`:type` gives you the type of an expression.

`:all` runs both `:eval` and `:step` on an expression.
