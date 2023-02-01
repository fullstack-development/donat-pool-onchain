module Main (main) where

import System.FilePath ((</>))

import Plutarch
import Ply.Plutarch

import Ext.Ply.Plutarch.TypedWriter
import Fundraising.Validator (fundraisingValidator)
import MintingPolicy.NFT (nftPolicy)
import MintingPolicy.VerToken (verTokenPolicy)
import Protocol.Validator
import Validator.AlwaysSucceeds (alwaysSucceedsValidator)

-- Compiles scripts and put them to files in the "compiled" folder
main :: IO ()
main = do
  writeTypedScriptTraced
    "NFT Minting Policy"
    "nftPolicy.plutus"
    nftPolicy
  writeTypedScriptTraced
    "Always Succeeds Validator"
    "alwaysSucceedsValidator.plutus"
    alwaysSucceedsValidator
  writeTypedScriptTraced
    "Protocol Validator"
    "protocolValidator.plutus"
    protocolValidator
  writeTypedScriptTraced
    "Verification token"
    "verTokenPolicy.plutus"
    verTokenPolicy
  writeTypedScriptTraced
    "Fundraising validator"
    "fundraisingValidator.plutus"
    fundraisingValidator
