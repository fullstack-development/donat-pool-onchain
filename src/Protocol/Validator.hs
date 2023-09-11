{-# LANGUAGE OverloadedRecordDot #-}

module Protocol.Validator where

import Ext.Plutus.MinAda
import Fundraising.Datum
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#+))
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model
import Protocol.Redeemer
import Shared.Checks
import Shared.ScriptContextV2

protocolValidator :: ClosedTerm (PProtocol :--> PValidator)
protocolValidator = plam $ \protocol datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PProtocolDatum datm
  (red, _) <- ptryFrom @PProtocolRedeemer redm
  txInfo <- plet $ getCtxInfoForSpending # ctx
  pmatch red $ \case
    PUpdateProtocolConfig redData -> popaque . unTermCont $ do
      protocolOutput <- pletC $ getOnlyOneOwnOutput # ctx
      checkSignedByManager dat txInfo
      checkUpdateProtocolDatum dat protocolOutput
      checkProtocolValueNotChanged protocol ctx protocolOutput
      pure $ pconstant ()
    PStartFundrise redData -> popaque . unTermCont $ do
      protocolOutput <- pletC $ getOnlyOneOwnOutput # ctx
      fundriseConfig <- pletC $ pfield @"_0" # redData
      fundriseAddress <- pletC $ pfield @"scriptAddress" # fundriseConfig
      fundriseOutput <- pletC $ getOutputByAddress # ctx # fundriseAddress
      frOutDatum' <- pletC $ inlineDatumFromOutput # fundriseOutput
      (frOutDatum, _) <- ptryFromC @PFundraisingDatum frOutDatum'
      checkProtocolValueNotChanged protocol ctx protocolOutput
      checkProtocolDatumNotChanged dat ctx protocolOutput
      checkFrTokensMinted fundriseConfig txInfo
      checkFundriseOutputDatum dat fundriseConfig frOutDatum
      checkFundriseOutputValue fundriseConfig fundriseOutput
      checkIsSignedBy "134" (pfield @"creatorPkh" # frOutDatum) txInfo
      pure $ pconstant ()
    PCloseProtocol _ -> popaque . unTermCont $ do
      checkSignedByManager dat txInfo
      checkNoOutputs ctx
      checkNftBurned protocol txInfo
      pure $ pconstant ()

checkSignedByManager :: Term s PProtocolDatum -> Term s (PAsData PTxInfo) -> TermCont s ()
checkSignedByManager datum txInfo = do
  managerPkh <- pletC $ extractPaymentPkhFromAddress #$ pfield @"managerAddress" # datum
  checkIsSignedBy "111" managerPkh txInfo

checkUpdateProtocolDatum :: Term s PProtocolDatum -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolDatum inDatum txOut = do
  let outDatum' = inlineDatumFromOutput # txOut
  (outDatum, _) <- ptryFromC @PProtocolDatum outDatum'
  pguardC "112" (pfield @"managerAddress" # inDatum #== pfield @"managerAddress" # outDatum)
  pguardC "113" (pfield @"tokenOriginRef" # inDatum #== pfield @"tokenOriginRef" # outDatum)
  pguardC "114" (pnot #$ inDatum #== outDatum)
  checkOutputDatumFields outDatum

checkOutputDatumFields :: Term s PProtocolDatum -> TermCont s ()
checkOutputDatumFields outDatum' = do 
  outDatum <- pletFieldsC @'["minAmount", "maxAmount", "minDuration", "maxDuration", "protocolFee"] outDatum'
  pguardC "128" (minTxOut #<= outDatum.minAmount)
  pguardC "129" (pfromData outDatum.minAmount #< pfromData outDatum.maxAmount)
  pguardC "130" (pconstant 1 #< pfromData outDatum.minDuration)
  pguardC "131" (pfromData outDatum.minDuration #< pfromData outDatum.maxDuration)
  pguardC "132" (pconstant 0 #<= pfromData outDatum.protocolFee #&& pfromData outDatum.protocolFee #< pconstant 100)

checkProtocolValueNotChanged :: Term s PProtocol -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkProtocolValueNotChanged protocol ctx txOut = do
  let inValue = getOwnInputValue # ctx
  let outValue = pfield @"value" # txOut
  pguardC "115" (inValue #== outValue)
  let threadTokenAmount = pvalueOf # outValue # protocolSymbol protocol # protocolToken protocol
  pguardC "116" (threadTokenAmount #== 1)

checkNftBurned :: Term s PProtocol -> Term s (PAsData PTxInfo) -> TermCont s ()
checkNftBurned protocol = checkNftMinted "123" (-1) (protocolSymbol protocol) (protocolToken protocol)

checkProtocolDatumNotChanged :: Term s PProtocolDatum -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkProtocolDatumNotChanged inDatum ctx txOut = do
  let outDatum' = inlineDatumFromOutput # txOut
  (outDatum, _) <- ptryFromC @PProtocolDatum outDatum'
  pguardC "117" (inDatum #== outDatum)

checkFrTokensMinted :: Term s PFundriseConfig -> Term s (PAsData PTxInfo) -> TermCont s ()
checkFrTokensMinted frConfig = do
  checkNftMinted "124" 1 (pfield @"verCurrencySymbol" # frConfig) (pfield @"verTokenName" # frConfig)
  checkNftMinted "125" 1 (pfield @"threadCurrencySymbol" # frConfig) (pfield @"threadTokenName" # frConfig)

checkFundriseOutputDatum ::
  Term s PProtocolDatum ->
  Term s PFundriseConfig ->
  Term s PFundraisingDatum ->
  TermCont s ()
checkFundriseOutputDatum pDatum frConfig frOutDatum = do
  frDatum <- pletFieldsC @["frTitle", "frAmount", "frDeadline", "managerAddress"] frOutDatum
  startedAt <- pletC $ pfield @"startedAt" # frConfig
  protocolDatum <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "managerAddress", "protocolFee"] pDatum
  
  pguardC "118" (pfield @"frFee" # frOutDatum #== protocolDatum.protocolFee)
  pguardC
    "119"
    ( pfromData protocolDatum.minAmount #<= pfromData frDatum.frAmount
        #&& pfromData frDatum.frAmount #<= pfromData protocolDatum.maxAmount
    )
  checkPermittedDuration protocolDatum.minDuration protocolDatum.maxDuration startedAt frDatum.frDeadline
  pguardC "127" (protocolDatum.managerAddress #== frDatum.managerAddress)
  pguardC "133" (plengthBS # (pfromData frDatum.frTitle) #<= descStringSize)

checkFundriseOutputValue :: Term s PFundriseConfig -> Term s PTxOut -> TermCont s ()
checkFundriseOutputValue frConfig frTxOut = do
  value <- pletC $ pfield @"value" # frTxOut
  adaAmount <- pletC $ plovelaceValueOf # value
  checkNftIsInValue "120" (pfield @"verCurrencySymbol" # frConfig) (pfield @"verTokenName" # frConfig) value
  checkNftIsInValue "121" (pfield @"threadCurrencySymbol" # frConfig) (pfield @"threadTokenName" # frConfig) value
  pguardC "122" (adaAmount #== minTxOut #+ minTxOut)

getProtocolDatumFromReferenceUtxo :: Term s (PProtocol :--> PScriptContext :--> PProtocolDatum)
getProtocolDatumFromReferenceUtxo = phoistAcyclic $
  plam $ \protocolToken ctx -> P.do
    token <- pletFields @["protocolCurrency", "protocolTokenName"] protocolToken
    getProtocolDatumFromReferenceUtxoByToken # token.protocolCurrency # token.protocolTokenName # ctx
    
getProtocolDatumFromReferenceUtxoByToken :: Term s (PCurrencySymbol :--> PTokenName :--> PScriptContext :--> PProtocolDatum)
getProtocolDatumFromReferenceUtxoByToken = phoistAcyclic $
  plam $ \protocolCs protocolTn ctx -> P.do
    protocolInput <- plet $ getOnlyOneRefInputByToken # protocolCs # protocolTn # ctx
    (protocolDatum, _) <- ptryFrom @PProtocolDatum $ inlineDatumFromOutput # protocolInput
    protocolDatum