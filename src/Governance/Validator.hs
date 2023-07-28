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
            -- protocol and psm - are the reference inputs
            checkNftIsInValue "1301" systemCurrency governanceThreadTokenName inValue
            redData <- pletFieldsC @["_0", "_1", "_2", "_3"] redData'

            currentProtocolDatum <- pletC $ getProtocolDatumFromReferenceUtxo # protocol # ctx
            let managerPkh = pfield @"managerPkh" # currentProtocolDatum
            inputDatum <- pletFieldsC @["quorum", "fee"] dat

            checkProposal redData._0 currentProtocolDatum txInfo
            checkProposalFeePayment inputDatum.fee managerPkh txInfo
            checkProposalOutput ctx redData._0 redData._1 redData._2 redData._3 inputDatum.quorum
 
            pure $ pconstant ()

checkProposal :: 
  Term s PProposalParameters 
  -> Term s PProtocolDatum
  -> Term s (PAsData PTxInfo) 
  -> TermCont s ()
checkProposal proposalParams currentProtocolDatum txInfo = do
      proposal <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "protocolFee"] proposalParams
      currentDatum <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "protocolFee"] currentProtocolDatum
      let similarConfigs = 
            (proposal.minAmount #== currentDatum.minAmount)
            #&& (proposal.maxAmount #== currentDatum.maxAmount)
            #&&( proposal.minDuration #== currentDatum.minDuration)
            #&& (proposal.maxDuration #== currentDatum.maxDuration)
            #&& (proposal.protocolFee #== currentDatum.protocolFee)
      pguardC  "1302" $ pnot # similarConfigs
    
checkProposalFeePayment :: 
  Term s PInteger 
  -> Term s PPubKeyHash 
  -> Term s (PAsData PTxInfo)
  -> TermCont s ()
checkProposalFeePayment proposalFee managerPkh txInfo = do
  pguardC "1304" $ minTxOut #<= proposalFee
  pguardC "1305" $ pubKeyContainsAmountOutput # managerPkh # txInfo # proposalFee

checkProposalOutput :: 
  Term s PScriptContext
  -> Term s PProposalParameters
  -> Term s PAddress 
  -> Term s PCurrencySymbol
  -> Term s PCurrencySymbol
  -> Term s PInteger
  -> TermCont s ()
checkProposalOutput txInfo proposal proposalAddress threadCur verCur quorum = do
  proposalOutput <- pletC $ getOutputByAddress # txInfo # proposalAddress
  outValue <- pletC $ pfield @"value" # proposalOutput
  adaAmount <- pletC $ Value.plovelaceValueOf # outValue
  pguardC "1306" (adaAmount #== minTxOut)

  outputNonAdaValue <- pletC $ Value.pforgetPositive $ Value.pnoAdaValue # outValue
  expectedTokensValue <- pletC $ 
    Value.psingleton # threadCur # proposalThreadTokenName # 1 
    <> Value.psingleton # verCur # proposalVerTokenName # 1
  pguardC "1307" (outputNonAdaValue #== expectedTokensValue)

  outputDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
  (outputDatum, _) <- tcont $ ptryFrom @PProposalDatum outputDatum'
  outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum"] outputDatum
  pguardC "1308" $ outDatum.proposal #== proposal
  pguardC "1309" $ outDatum.quorum #== quorum
  pguardC "1310" $ (outDatum.for #== pdata 0) #&& (outDatum.against #== pdata 0)

governanceThreadTokenName :: Term s PTokenName
governanceThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "AdadaoGovernance")
