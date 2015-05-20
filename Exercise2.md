Exercise 2: `get`
=================

The second exercise is to write the `get` function, retrieving an element
from a heterogeneous list.

In the `glambda` repo, look in the `exercises` directory and open up
`Ex2.hs`. The code we've been discussing should appear.

Write a function `get :: HList tys -> Elem tys ty -> ty`. A correct
definition of this function will tickle GHC's bug #3927, when you get
a spurious warning about incomplete patterns.

If you have extra time left over at the end, start playing with glambda.
In the top level of the `glambda` repo, say

    > cabal configure
    > cabal build
    > ./dist/build/glam/glam

A brief manual for the Glamorous Glambda interpreter is toward the bottom
of the [README](README.md) for this repo. You'll find some test `.glam`
files in the `tests` directory. Load them with `:load tests/revapp.glam`
for instance. (No quotes around filenames, please.)
