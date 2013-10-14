# Pure Pattern Matching

Refer to README in root of repository for explanation of the project concept.

This was the second attempt, using filters to isolate peaks and valleys and to match patterns.
In this design, patterns are matched against the set of pricebars without regard to new data
arriving.

Build, run unit tests, and create documentation with:
* `cabal configure --enable-tests`
* `cabal build`
* `cabal test`
* `cabal haddock --executables`

Two programs are built:
* pure-matcher
    + Command-line program to count matched patterns
    + run with `dist/build/pure-matcher/pure-matcher test#`
      where # is a digit from 1 to 5
* pure-viewer
    + Uses OpenGL to draw price bars as candlesticks, and matched patterns
    + run with `dist/build/pure-viewer/pure-viewer 400` where the 400 is the number of price bars to use

**NOTE:** The programs must be run from the directory containing the source
code in order to find their datasource because
currently it is hardcoded to be a file in the ../../data directory. Various other parameters are also hardcoded.

To view the documentation, open the files
`dist/doc/html/pure-matcher/pure-viewer/index.html` or
`dist/doc/html/pure-matcher/pure-matcher/index.html`.

Its original purpose is no longer relevant, but I may still work on it for practice. There are countless ways to improve it.
