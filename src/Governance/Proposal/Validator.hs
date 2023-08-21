{-# LANGUAGE OverloadedRecordDot #-}
module Governance.Proposal.Validator where

import Ext.Plutus.MinAda
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import Plutarch.Num (pnegate, (#+), (#-))
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Shared.Checks (checkNftIsInValue, checkIsSignedBy, checkNftMinted, checkNoOutputs)
import Shared.ScriptContextV2
import Governance.Datum (PGovernanceDatum(PGovernanceDatum))
import Governance.Proposal.Redeemer ( PProposalRedeemer (PVote))
import Data.Data
import Governance.Proposal.Datum
import PlutusLedgerApi.V1 (PubKeyHash)
import Governance.Proposal.Model (PProposal)

-- TODO: error codes
proposalValidator :: ClosedTerm (PProposal :--> PValidator)
proposalValidator = phoistAcyclic $
  plam $ \gov dat' rdm' ctx -> P.do
    (dat, _) <- ptryFrom @PProposalDatum dat'
    (red, _) <- ptryFrom @PProposalRedeemer rdm'
    txInfo <- plet $ pfield @"txInfo" # ctx
    input <- plet $ getOwnInputOrTraceError # ctx
    inValue <- plet $ pfield @"value" # input
    -- let systemCurrency = pfield @"protocolCurrency" # protocol
    
    pmatch red $ \case
        PVote redData' -> popaque . unTermCont $ do
            -- checkNftIsInValue "1201" systemCurrency governanceThreadTokenName inValue
            redData <- pletFieldsC @["_0", "_1"] redData'
            inputDatum <- pletFieldsC @["for", "against", "quorum"] dat
            pguardC  "1203" $ pfromData inputDatum.for #+ inputDatum.against #< inputDatum.quorum
            
            -- currentProtocolConf <- pletC $ getProtocolConfigFromReferenceUtxo # protocol # ctx
            -- let managerPkh = pfield @"pcAutomatedActionsPkh" # currentProtocolConf
            -- inputDatum <- pletFieldsC @["quorum", "fee"] dat

            -- checkProposal redData.proposal protocol currentProtocolConf ctx
            -- checkProposalFeePayment inputDatum.fee managerPkh txInfo
            -- checkProposalOutput txInfo redData.proposal redData.proposalAddress redData.proposalThreadCurrency redData.proposalVerCurrency inputDatum.quorum
 
            pure $ pconstant ()

checkQuorum for against quorum = do
     pguardC  "1203" $ for #+ against #< quorum