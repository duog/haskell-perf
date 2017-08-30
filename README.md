# Summary #

Tool for measuring haddock performance across ghc and haddock versions.

To use:
* stack setup
* stack build
* edit run.sh to match your environment, and choose your haddock commits.
* ./run.sh <cabal install parameters>. For example, ./run.sh yesod lens --enable-tests
* stack exec haskell-perf > haskell-perf.csv
