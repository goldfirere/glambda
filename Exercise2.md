Exercise 2: `subst`
===================

The second exercise is to write the `subst` function, that performs
substitution of one expression into another.

To ensure the proper setup for this task, go into the `glambda` repo you've
cloned and say

    > git checkout ex2

This will put you on the `ex2` tag, which has everything in just the right
state for this exercise.

Just to make sure everything is still humming along nicely at this point,
go ahead and do a

    > cabal configure
    > cabal build

(You won't need to enable testing for these exercises.)

The final program should build without any problems. Of course, it won't
be able to apply functions, as that involves substitution.

Write your evaluator in `src/Language/Glambda/Shift.hs`. You will see an `subst`
function stubbed out in that file. Fill it in.

Useful tips:
  * To start an interactive session with the glambda package loaded, use
    `cabal repl` in the `glambda` directory. If that somehow isn't the
    setup you want, `cabal exec XXX` runs command `XXX` with your sandbox
    available. (For example, you can say `cabal exec ghci` for a sandbox-aware
    GHCi session or `cabal exec bash` to make a prompt where every call to
    GHC or its utilities uses your sandboxed set of packages.)

  * The `subst` function has a structure very similar to the `shift` function
    higher up in that file. In particular, you will need to use the `Length`
    type in exactly the same way that `shift` does. (Why?)

  * If you get stuck just starting, try `git checkout ex2-hint`. It will
    fill in a little more structure of the `subst` function for you and will
    hopefully be enough to get you moving.

  * Don't worry about adhering to the definition of de Bruijn substitution
    on the slide -- the types in glambda force you to do it a slightly
    different way (with the same result, of course).

  * You will *not* need to edit any file other than `Shift.hs`.
