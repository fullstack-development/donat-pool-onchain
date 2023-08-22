{-# LANGUAGE OverloadedRecordDot #-}

module Governance.Validator where

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
import Governance.Redeemer (PGovernanceRedeemer(..))
import Governance.Proposal.Datum 
import PlutusLedgerApi.V1 (PubKeyHash)
import Protocol.Model (PProtocolConfig, PProtocol, ProtocolConfig (..))
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus
import Governance.Proposal.Model (proposalThreadTokenName, proposalVerTokenName, PProposal)
import Protocol.Validator (getProtocolDatumFromReferenceUtxo)
import Protocol.Datum (PProtocolDatum)
import qualified Protocol.Model as Proposal
import MintingPolicy.NFT (checkUTxOSpent)
import Ext.Plutarch.Extra.Time (minutesToPosixDuration, addTimes)
import Plutarch.Extra.Interval (pinterval)

governanceValidator :: ClosedTerm (PProtocol :--> PValidator)
governanceValidator = phoistAcyclic $
  plam $ \protocol dat' rdm' ctx -> P.do
    (dat, _) <- ptryFrom @PGovernanceDatum dat'
    (red, _) <- ptryFrom @PGovernanceRedeemer rdm'
    txInfo <- plet $ pfield @"txInfo" # ctx
    input <- plet $ getOwnInputOrTraceError # ctx
    inValue <- plet $ pfield @"value" # input
    let systemCurrency = pfield @"protocolCurrency" # protocol
    
    pmatch red $ \case
        PCreateProposal redData' -> popaque . unTermCont $ do
            -- redData: proposalParams, proposalAddress, proposalThreadCs, proposalVerCs, proposalStartedAt
            redData <- pletFieldsC @["_0", "_1", "_2", "_3", "_4"] redData' 
            checkNftIsInValue "801" systemCurrency governanceThreadTokenName inValue
            checkProposalOutput protocol ctx redData._0 redData._1 redData._2 redData._3 redData._4 dat
            checkGovernanceOutput ctx dat inValue
            pure $ pconstant ()

checkGovernanceOutput :: 
  Term s PScriptContext
  -> Term s PGovernanceDatum
  -> Term s SortedPositiveValue
  -> TermCont s ()
checkGovernanceOutput ctx inDatum inValue = do
  govOutput <- pletC $ getOnlyOneOwnOutput # ctx
  govOutDatum' <- pletC $ inlineDatumFromOutput # govOutput
  (outDatum, _) <- ptryFromC @PGovernanceDatum govOutDatum'
  pguardC  "812" $ (inDatum #== outDatum) 
  outValue <- pletC $ pfield @"value" # govOutput
  pguardC  "813" $ (inValue #== outValue) 


checkProposalOutput :: 
  Term s PProtocol
  -> Term s PScriptContext
  -> Term s PProposalParameters
  -> Term s PAddress 
  -> Term s PCurrencySymbol
  -> Term s PCurrencySymbol
  -> Term s (PAsData PPOSIXTime)
  -> Term s PGovernanceDatum
  -> TermCont s ()
checkProposalOutput protocol ctx proposal proposalAddress threadCs verCs startedAt govInputDatum' = do
  checkProposalCanChangeProtocol protocol proposal ctx
  proposalOutput <- pletC $ getOutputByAddress # ctx # proposalAddress
  govInputDatum <- pletFieldsC @["quorum", "fee", "duration"] govInputDatum'
  checkProposalValue proposalOutput govInputDatum.fee threadCs verCs
  checkProposalDatum proposal proposalOutput govInputDatum.quorum govInputDatum.duration startedAt ctx

checkProposalValue :: 
  Term s PTxOut
  -> Term s PInteger
  -> Term s PCurrencySymbol
  -> Term s PCurrencySymbol
  -> TermCont s ()
checkProposalValue proposalOutput fee threadCs verCs = do
  proposalOutValue <- pletC $ pfield @"value" # proposalOutput
  adaAmount <- pletC $ Value.plovelaceValueOf # proposalOutValue
  pguardC "804" $ minTxOut #<= fee
  pguardC "806" (adaAmount #== minTxOut #+ fee)
  outputNonAdaValue <- pletC $ Value.pforgetPositive $ Value.pnoAdaValue # proposalOutValue
  expectedTokensValue <- pletC $ 
    Value.psingleton # threadCs # proposalThreadTokenName # 1 
    <> Value.psingleton # verCs # proposalVerTokenName # 1
  pguardC "807" (outputNonAdaValue #== expectedTokensValue)

checkProposalDatum :: 
  Term s PProposalParameters
  -> Term s PTxOut 
  -> Term s (PAsData PInteger) 
  -> Term s PInteger
  -> Term s (PAsData PPOSIXTime)
  -> Term s PScriptContext
  -> TermCont s ()
checkProposalDatum proposal proposalOutput quorum duration startedAt ctx = do
  proposalOutDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
  (proposalOutDatum, _) <- tcont $ ptryFrom @PProposalDatum proposalOutDatum'
  outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "deadline", "applied"] proposalOutDatum
  pguardC "808" $ outDatum.proposal #== proposal
  pguardC "809" $ outDatum.quorum #== quorum
  pguardC "810" $ (outDatum.for #== pdata 0) #&& (outDatum.against #== pdata 0)
  checkUTxOSpent outDatum.policyRef ctx
  pguardC "814" $ outDatum.applied #== pdata 0
  txInfo <- pletC $ pfield @"txInfo" # ctx
  checkIsSignedBy "805" (extractPaymentPkhFromAddress # outDatum.initiator) txInfo
  checkPermittedDuration duration startedAt outDatum.deadline

checkProposalCanChangeProtocol :: 
  Term s PProtocol 
  -> Term s PProposalParameters 
  -> Term s PScriptContext 
  -> TermCont s ()
checkProposalCanChangeProtocol protocol proposal' ctx = do
  protocolDatum' <- pletC $ getProtocolDatumFromReferenceUtxo # protocol # ctx
  protocolDatum <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "protocolFee"] protocolDatum'
  proposal <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "protocolFee"] proposal'
  pguardC "802" $ pnot #$
    (protocolDatum.minAmount #== proposal.minAmount
    #&& protocolDatum.maxAmount #== proposal.maxAmount
    #&& protocolDatum.minDuration #== proposal.minDuration
    #&& protocolDatum.maxDuration #== proposal.maxDuration
    #&& protocolDatum.protocolFee #== proposal.protocolFee)

checkPermittedDuration ::
  Term s PInteger ->
  Term s (PAsData PPOSIXTime) ->
  Term s (PAsData PPOSIXTime) ->
  TermCont s ()
checkPermittedDuration durationInMinutes startedAt deadline = do
  let proposalDuration = pinterval # startedAt # deadline
  let expectedEndTime = minutesToPosixDuration # durationInMinutes # startedAt
  let expectedDuration = pinterval # startedAt # expectedEndTime
  pguardC "1315" (proposalDuration #== expectedDuration)

governanceThreadTokenName :: Term s PTokenName
governanceThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolGovernance")

getGovernanceDatumFromReferenceUtxo :: Term s (PCurrencySymbol :--> PScriptContext :--> PGovernanceDatum)
getGovernanceDatumFromReferenceUtxo = phoistAcyclic $
  plam $ \systemCurrency txInfo -> P.do
    govInput <- plet $ getOnlyOneRefInputByToken # systemCurrency # governanceThreadTokenName # txInfo
    (govDatum, _) <- ptryFrom @PGovernanceDatum $ inlineDatumFromOutput # govInput
    govDatum
