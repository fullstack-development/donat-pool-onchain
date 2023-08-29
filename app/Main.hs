module Main (main) where

import Ext.Ply.Plutarch.TypedWriter
import FeePool.FeePoolInfo.Validator (feePoolInfoValidator)
import FeePool.Validator (feePoolValidator)
import Fundraising.Validator (fundraisingValidator)
import Governance.Proposal.Validator (proposalValidator)
import Governance.Validator (governanceValidator)
import MintingPolicy.Governance (governancePolicy)
import MintingPolicy.NFT (nftPolicy)
import MintingPolicy.Proposal (proposalPolicy)
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
  writeTypedScriptTraced
    "Proposal Thread Minting Policy"
    "proposalPolicy.plutus"
    proposalPolicy
  writeTypedScriptTraced
    "FeePoolInfo validator"
    "feePoolInfo.plutus"
    feePoolInfoValidator
  writeTypedScriptTraced
    "FeePool validator"
    "feePool.plutus"
    feePoolValidator
