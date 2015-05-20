Exercise 3: `Arith` and `Cond`
==============================

The last exercise is to add the `Arith` and `Cond` constructors to `Exp`.

To ensure the proper setup for this task, go into the `glambda` repo you've
cloned and say

    > git checkout ex3

This will put you on the `ex3` tag, which has everything in just the right
state for this exercise.

Just to make sure everything is still humming along nicely at this point,
go ahead and do a

    > cabal configure
    > cabal build

(You won't need to enable testing for these exercises.)

The final program should build without any problems. Of course, it won't be
able to deal with arithmetic expressions or conditionals.

1. First, add `Arith` and `Cond` constructors to the `Exp` type in
`src/Language/Glambda/Exp.hs`. Use `UArith` and `UCond` from `UExp` (in
`src/Language/Glambda/Unchecked.hs`) as templates.
You will also need to update several functions in `Exp` to deal with
the new constructors. (See `src/Language/Glambda/Pretty.hs` for functions
to help with pretty-printing your new forms.)

2. Add clauses for your new forms in `shift` and `subst` in the `Shift`
module. These should be very straightforward.

3. Add clauses for your new forms in the big-step and small-step evaluators,
in `Eval`. (You can skip small-step -- which is a bit harder -- if you want
to see something working quickly.)

4. Edit the `Check` module to uncomment the region that type-checks arithmetic
and conditional expressions. Feel free to stare at this code if you want
to understand it, but it's a bit beyond the scope of today's workshop.

5. Marvel at your handiwork, adding `1 + 1` to get `2 : Int`.

Remember:
  * To start an interactive session with the glambda package loaded, use
    `cabal repl` in the `glambda` directory. If that somehow isn't the
    setup you want, `cabal exec XXX` runs command `XXX` with your sandbox
    available. (For example, you can say `cabal exec ghci` for a sandbox-aware
    GHCi session or `cabal exec bash` to make a prompt where every call to
    GHC or its utilities uses your sandboxed set of packages.)
