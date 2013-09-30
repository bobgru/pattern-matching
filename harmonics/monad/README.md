# Monadic Pattern Matching

Refer to README in root of repository for explanation of the project concept.

This was the first attempt, using `State` monads to track peaks and valleys and to record the work-in-progress of matching patterns.

Build with `make` which will run unit tests automatically. **NOTE:** On Windows, if there is no `make` utility,
the command lines from the makefile must be run manually. 

Two programs are built:
* SampleApp
    + Command-line program to count matched patterns
    + run with `./SampleApp chunk` or `./SampleApp no-chunk` (the former runs much faster than the latter)
* SampleGraphApp
    + Uses OpenGL to draw price bars as candlesticks--not matched patterns

The datasource is hardcoded to be a file in the ../../data directory. Various other parameters are also hardcoded.

Its original purpose is no longer relevant, but I may still work on it for practice. There are countless ways to improve it.
