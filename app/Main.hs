module Main (main) where

import Ext.Ply.Plutarch.TypedWriter
import Fundraising.Validator (fundraisingValidator)
import MintingPolicy.NFT (nftPolicy)
import MintingPolicy.VerToken (verTokenPolicy)
import Plutarch
import Ply.Plutarch
import Protocol.Validator (protocolValidator)

-- Compiles scripts and put them to files in the "compiled" folder
main :: IO ()
main = do
  writeTypedScriptTraced
    "NFT Minting Policy"
    "nftPolicy.plutus"
    nftPolicy
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
