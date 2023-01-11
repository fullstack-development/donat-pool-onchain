module Main (main) where

import System.FilePath ((</>))

import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing))
import Ply.Plutarch

import MintingPolicy.NFT (nftPolicy)

-- TODO: change the directory path when we know where to put compiled scripts
-- Compiles all the scripts and put them to files in the "compiled"s folder
main :: IO ()
main =
  writeTypedScript
    (Config {tracingMode = DoTracing})
    "NFT Minting Policy"
    ("compiled" </> "nftPolicy.plutus")
    nftPolicy
