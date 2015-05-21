Exercise 3: `eval`, `apply`, and `cond`
=======================================

The last exercise is to write the `eval`, `apply`, and `cond` functions.

To ensure the proper setup for this task, go into the `glambda` repo you've
cloned and say

    > git checkout ex3

This will put you on the `ex3` branch, which has everything in just the right
state for this exercise.

Just to make sure everything is still humming along nicely at this point,
go ahead and do a

    > cabal configure
    > cabal build

(You won't need to enable testing for these exercises.)

The final program should build without any problems. Of course, it doesn't
have an evaluator! But you should still be able to do things, as long as
those things don't require using the big-step evaluator function application.
(Both the big-step and small-step evaluators use the same -- now missing --
code for function application.) To test, you can run `glam`:

    > ./dist/build/glam/glam
    ...
    λ> :step \x:Int.x
    λ#. #0 : Int -> Int
    λ#. #0 : Int -> Int
    λ> :quit

Note how these instructions use `:step` to avoid using the big-step evaluator.

Write your evaluator in `src/Language/Glambda/Eval.hs`. You will see `apply`,
`cond`, and `eval` functions stubbed out in that file. Fill them in.

Useful tips:
  * To start an interactive session with the glambda package loaded, use
    `cabal repl` in the `glambda` directory. If that somehow isn't the
    setup you want, `cabal exec XXX` runs command `XXX` with your sandbox
    available. (For example, you can say `cabal exec ghci` for a sandbox-aware
    GHCi session or `cabal exec bash` to make a prompt where every call to
    GHC or its utilities uses your sandboxed set of packages.)

  * You will need to work closely with the `Exp` type, defined in
    `src/Language/Glambda/Exp.hs`. You will also need the `Val` type,
    also defined in `Exp.hs`. If it's more convenient, you can also
    use the Haddock docs at https://hackage.haskell.org/package/glambda

  * You may want to look at `step`, the small-step stepper for inspiration.

  * You will also need to use substitution. Use

        subst :: Exp ctx s -> Exp (s ': ctx) t -> Exp ctx t

  to substitute the first expression in for the 0'th variable of the second.
  This function is defined in the `Language.Glambda.Shift` module.

  * You will *not* need to edit any file other than `Eval.hs`.
