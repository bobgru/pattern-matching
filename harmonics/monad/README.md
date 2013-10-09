# Monadic Pattern Matching

Refer to README in root of repository for explanation of the project concept.

This was the first attempt, using `State` monads to track peaks and valleys and to record the work-in-progress of matching patterns.

Build and run unit tests with:
* `cabal configure --enable-tests`
* `cabal build`
* `cabal test`

Two programs are built:
* monadic-matcher
    + Command-line program to count matched patterns
    + run with `dist/build/monadic-matcher/monadic-matcher chunk`
      or `dist/build/monadic-matcher/monadic-matcher no-chunk` (the former runs much faster than the latter)
* monadic-viewer
    + Uses OpenGL to draw price bars as candlesticks--not matched patterns
    + run with `dist/build/monadic-viewer/monadic-viewer 200` where the 200 is the number of price bars to use

**NOTE:** The programs must be run from the _monad_ directory in order to find their datasource because
currently it is hardcoded to be a file in the ../../data directory. Various other parameters are also hardcoded.

Its original purpose is no longer relevant, but I may still work on it for practice. There are countless ways to improve it.
