# donat-pool-contracts

[Technical specification](https://docs.google.com/document/d/13nBgWRqfUBwJsOIgqEbwhmOVXEVJKjvgJXWmTwdsM0A/edit?usp=sharing)

[One transaction workflow](https://docs.google.com/document/d/1A61UgkiTQVYdvCzx8gu9hwZy-akq0bYww9-vIRQaIfI/edit?usp=sharing)

# Dev Notes

Install the following versions of GHC and cabal:

```
GHC 9.2.5
Cabal 3.6.2.0
```

run `nix develop` and then `cabal build all` to compile project.

# Compiling Plutus scripts

For compiling Plutus scripts make sure that you have `compiled` folder in the project root and run `cabal run`.
