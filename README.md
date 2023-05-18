# donat-pool-onchain

## Dev Notes

Install the following versions of GHC and cabal:

```
GHC 9.2.5
Cabal 3.6.2.0
```

run `nix develop` and then `cabal build all` to compile project.

## Compiling Plutus scripts

For compiling Plutus scripts make sure that you have `compiled` folder in the project root and run `cabal run compile`.

# Copy compiled scripts to offchain directory

After scripts are compiled you can copy them to offchain project (as the path to offchain project is hardcoded, this step is helpful only if the onchain and offchain projects are located in the same parent directory):

`cabal run copy-scripts`