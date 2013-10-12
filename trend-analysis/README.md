# Trend analysis with account modeling

Refer to README in root of repository for explanation of the project concept.

The main goal of this program was to model a brokerage account.

Build, run unit tests, and create documentation with:
* `cabal configure --enable-tests`
* `cabal build`
* `cabal test`
* `cabal haddock --executables`

One program is built:
* trend-analyzer
    + Command-line program to simulate trades in an account with
      data from a historical price feed
    + run with `dist/build/trend-analyzer/trend-analyzer`
    + run with `--help` to see options

To see the documentation, open in a browser the file `dist/doc/html/trend-analyzer/trend-analyzer/index.html`.

**NOTE:** The program must be run from the _trend-analyzer_ directory in order to find its default datasource because
currently it is hardcoded to be a file in the ../data directory. Various other parameters are also hardcoded.
