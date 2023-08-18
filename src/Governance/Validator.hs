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
import Governance.Proposal.Model (proposalThreadTokenName, proposalVerTokenName)
import Protocol.Validator (getProtocolDatumFromReferenceUtxo)
import Protocol.Datum (PProtocolDatum)
import qualified Protocol.Model as Proposal
import MintingPolicy.NFT (checkUTxOSpent)

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
            checkNftIsInValue "1301" systemCurrency governanceThreadTokenName inValue
            -- redData: proposalParams, proposalAddress, proposalThreadCs, proposalVerCs, proposalStartedAt
            redData <- pletFieldsC @["_0", "_1", "_2", "_3", "_4"] redData' 

            -- TODO: check that protocol changed
            -- currentProtocolConf <- pletC $ getProtocolConfigFromReferenceUtxo # protocol # txInfo
            -- checkProposal redData._0 protocol currentProtocolConf txInfo

            checkProposalOutput ctx redData._0 redData._1 redData._2 redData._3 redData._4 dat
            checkGovernanceOutput ctx dat inValue
            pure $ pconstant ()

-- checkProposal :: 
--   Term s PProposalParameters 
--   -> Term s PStablecoinProtocolThreadToken 
--   -> Term s PProtocolConfiguration
--   -> Term s (PAsData PTxInfo) 
--   -> TermCont s ()
-- checkProposal proposalParams protocol currentProtocolConf txInfo = do
--   pmatchC proposalParams >>= \case
--     protocolProposal@(PProtocolProposal _) -> do
--       let currentProtocol = protocolConfToProposal # currentProtocolConf
--       pguardC  "1302" $ pnot # (pcon protocolProposal #== currentProtocol) 
--     psmProposal@(PPsmProposal _) ->  do
--       currentPsmDatum <- pletC $ getPsmDatumFromReferenceUtxo # protocol # txInfo
--       let currentPsm = psmDatumToProposal # currentPsmDatum
--       pguardC  "1303" $ pnot # (pcon psmProposal #== currentPsm) 

checkProposalOutput :: 
  Term s PScriptContext
  -> Term s PProposalParameters
  -> Term s PAddress 
  -> Term s PCurrencySymbol
  -> Term s PCurrencySymbol
  -> Term s (PAsData PPOSIXTime)
  -> Term s PGovernanceDatum
  -> TermCont s ()
checkProposalOutput ctx proposal proposalAddress threadCur verCur startedAt govInputDatum' = do
  proposalOutput <- pletC $ getOutputByAddress # ctx # proposalAddress
  govInputDatum <- pletFieldsC @["quorum", "fee", "duration"] govInputDatum'
  proposalOutValue <- pletC $ pfield @"value" # proposalOutput
  adaAmount <- pletC $ Value.plovelaceValueOf # proposalOutValue
  pguardC "1304" $ minTxOut #<= govInputDatum.fee
  pguardC "1306" (adaAmount #== minTxOut #+ govInputDatum.fee)

  outputNonAdaValue <- pletC $ Value.pforgetPositive $ Value.pnoAdaValue # proposalOutValue
  expectedTokensValue <- pletC $ 
    Value.psingleton # threadCur # proposalThreadTokenName # 1 
    <> Value.psingleton # verCur # proposalVerTokenName # 1
  pguardC "1307" (outputNonAdaValue #== expectedTokensValue)

  proposalOutDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
  (proposalOutDatum, _) <- tcont $ ptryFrom @PProposalDatum proposalOutDatum'
  outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "deadline", "applied"] proposalOutDatum
  pguardC "1308" $ outDatum.proposal #== proposal
  pguardC "1309" $ outDatum.quorum #== govInputDatum.quorum
  pguardC "1310" $ (outDatum.for #== pdata 0) #&& (outDatum.against #== pdata 0)
  pguardC "1314" $ outDatum.applied #== pdata 0
  -- checkPermittedDuration govInputDatum.minDuration govInputDatum.maxDuration startedAt outDatum.deadline
  txInfo <- pletC $ pfield @"txInfo" # ctx
  checkIsSignedBy "1305" (extractPaymentPkhFromAddress # outDatum.initiator) txInfo
  checkUTxOSpent outDatum.policyRef ctx

checkGovernanceOutput :: 
  Term s PScriptContext
  -> Term s PGovernanceDatum
  -> Term s SortedPositiveValue
  -> TermCont s ()
checkGovernanceOutput ctx inDatum inValue = do
  govOutput <- pletC $ getOnlyOneOwnOutput # ctx
  govOutDatum' <- pletC $ inlineDatumFromOutput # govOutput
  (outDatum, _) <- ptryFromC @PGovernanceDatum govOutDatum'
  pguardC  "1312" $ (inDatum #== outDatum) 
  outValue <- pletC $ pfield @"value" # govOutput
  pguardC  "1213" $ (inValue #== outValue) 

-- checkPermittedDuration ::
--   Term s PInteger ->
--   Term s PInteger ->
--   Term s (PAsData PPOSIXTime) ->
--   Term s (PAsData PPOSIXTime) ->
--   TermCont s ()
-- checkPermittedDuration minDurationMinutes maxDurationMinutes startedAt deadline = do
--   let minDuration = minutesToPosixDuration # minDurationMinutes # startedAt
--   let maxDuration = minutesToPosixDuration # maxDurationMinutes # startedAt
--   let permittedDuration = pinterval # minDuration # maxDuration
--   pguardC "1315" (pmember # deadline # permittedDuration)

governanceThreadTokenName :: Term s PTokenName
governanceThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolGovernance")
