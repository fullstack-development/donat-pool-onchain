{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Protocol.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutarch.Extra.Time
import Ext.Plutus.MinAda
import Fundraising.Datum
import Fundraising.Model (byteStringSize)
import Generics.SOP
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.ByteString
import Plutarch.DataRepr
import Plutarch.Extra.Interval
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#+))
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Datum
import Protocol.Model
import Protocol.Redeemer
import Shared.Checks
import Shared.ScriptContextV2

-- TODO: Add config values validation for updateProtocol
-- validate config values:
-- -- minPoolSize >= minAdaTxOutAmount
-- -- maxPoolSize > minPoolSize
-- -- minDuration >= 1 day
-- -- maxDuration < minDuration
-- -- fee > 0 and < 100 %

protocolValidator :: ClosedTerm (PProtocol :--> PValidator)
protocolValidator = plam $ \protocol datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PProtocolDatum datm
  (red, _) <- ptryFrom @PProtocolRedeemer redm
  txInfo <- plet $ getCtxInfoForSpending # ctx
  pmatch red $ \case
    PUpdateProtocolConfig redData -> popaque . unTermCont $ do
      checkSignedByManager dat txInfo
      checkUpdateProtocolOutput protocol dat ctx
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
      checkIsSignedBy "131" (pfield @"creatorPkh" # frOutDatum) txInfo
      pure $ pconstant ()
    PCloseProtocol _ -> popaque . unTermCont $ do
      checkNoOutputs ctx
      checkNftBurned protocol txInfo
      checkSignedByManager dat txInfo
      pure $ pconstant ()

checkSignedByManager :: Term s PProtocolDatum -> Term s (PAsData PTxInfo) -> TermCont s ()
checkSignedByManager datum txInfo = do
  let managerPkh = pfield @"managerPkh" # datum
  checkIsSignedBy "111" managerPkh txInfo

checkUpdateProtocolOutput :: Term s PProtocol -> Term s PProtocolDatum -> Term s PScriptContext -> TermCont s ()
checkUpdateProtocolOutput protocol inDatum ctx = do
  let output = getOnlyOneOwnOutput # ctx
  checkUpdateProtocolDatum inDatum ctx output
  checkProtocolValueNotChanged protocol ctx output

checkUpdateProtocolDatum :: Term s PProtocolDatum -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolDatum inDatum ctx txOut = do
  let outDatum' = inlineDatumFromOutput # txOut
  (outDatum, _) <- ptryFromC @PProtocolDatum outDatum'
  pguardC "112" (pfield @"managerPkh" # inDatum #== pfield @"managerPkh" # outDatum)
  pguardC "113" (pfield @"tokenOriginRef" # inDatum #== pfield @"tokenOriginRef" # outDatum)
  pguardC "114" (pnot #$ inDatum #== outDatum)

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
  frDatum <- pletFieldsC @["frDesc", "frAmount", "frDeadline", "managerPkh", "frUrl"] frOutDatum
  startedAt <- pletC $ pfield @"startedAt" # frConfig
  protocolDatum <- pletFieldsC @["minAmount", "maxAmount", "minDuration", "maxDuration", "managerPkh", "protocolFee"] pDatum

  pguardC "118" (pfield @"frFee" # frOutDatum #== protocolDatum.protocolFee)
  pguardC
    "119"
    ( pfromData protocolDatum.minAmount #<= pfromData frDatum.frAmount
        #&& pfromData frDatum.frAmount #<= pfromData protocolDatum.maxAmount
    )
  checkPermittedDuration protocolDatum.minDuration protocolDatum.maxDuration startedAt frDatum.frDeadline
  pguardC "127" (protocolDatum.managerPkh #== frDatum.managerPkh)
  pguardC "128" (plengthBS # (pfromData frDatum.frUrl) #<= byteStringSize)
  pguardC "129" (plengthBS # (pfromData frDatum.frDesc) #<= byteStringSize)

checkFundriseOutputValue :: Term s PFundriseConfig -> Term s PTxOut -> TermCont s ()
checkFundriseOutputValue frConfig frTxOut = do
  let value = pfield @"value" # frTxOut
  let adaAmount = plovelaceValueOf # value
  checkNftIsInValue "120" (pfield @"verCurrencySymbol" # frConfig) (pfield @"verTokenName" # frConfig) value
  checkNftIsInValue "121" (pfield @"threadCurrencySymbol" # frConfig) (pfield @"threadTokenName" # frConfig) value
  pguardC "122" (adaAmount #== minTxOut #+ minTxOut)
