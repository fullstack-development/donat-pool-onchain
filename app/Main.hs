module Main (main) where

import Ext.Ply.Plutarch.TypedWriter
import Fundraising.Validator (fundraisingValidator)
import MintingPolicy.NFT (nftPolicy)
import MintingPolicy.VerToken (verTokenPolicy)
import Plutarch
import Ply.Plutarch
import Protocol.Validator (protocolValidator)
import Governance.Validator (governanceValidator)
import Governance.Proposal.Validator (proposalValidator)
import  MintingPolicy.Governance (governancePolicy)

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
  writeTypedScriptTraced
    "Governance validator"
    "governance.plutus"
    governanceValidator
  writeTypedScriptTraced
    "Proposal validator"
    "proposal.plutus"
    proposalValidator
  writeTypedScriptTraced
    "Governance Minting Policy"
    "governancePolicy.plutus"
    governancePolicy
