{-# LANGUAGE OverloadedRecordDot #-}

module StakingPool.Validator where

import Ext.Plutarch.Extra.Time 
import Ext.Plutus.MinAda (minAdaValue)
import MintingPolicy.VerToken (stakingPoolVerTokenName)
import qualified Plutarch.Api.V1.Value as Value
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num (pnegate, (#+), (#-))
import Plutarch.Prelude
import Protocol.Validator (getProtocolDatumFromReferenceUtxo)
import Shared.Checks 
import Shared.ScriptContextV2
import StakingPool.Datum (PStakingPoolDatum)
import StakingPool.Models (PStakingPool, stakingPoolThreadTokenName)
import StakingPool.Redeemer (ProviderPkh, PStakingPoolRedeemer(..))
import StakingPool.StakingPoolInfo.Datum (DaoTokensAmt, PStakingPoolInfoDatum)

stakingPoolValidator :: ClosedTerm (PStakingPool :--> PValidator)
stakingPoolValidator = plam $ \stakingPool dtm rdm ctx -> P.do
  (dat, _) <- ptryFrom @PStakingPoolDatum dtm
  (red, _) <- ptryFrom @PStakingPoolRedeemer rdm
  stPoolFields <- pletFields @["protocol", "verTokenCurrency", "govCurrency", "govTokenName"] stakingPool
  protocolCurrency <- plet $ pfield @"protocolCurrency" # stPoolFields.protocol
  inputEpoch <- plet $ pfield @"currentEpoch" # dat
  inputValue <- plet $ getOwnInputValue # ctx
  output <- plet $ getOnlyOneOwnOutput # ctx
  outputDatum' <- plet $ inlineDatumFromOutput # output
  (outputDatum, _) <- ptryFrom @PStakingPoolDatum outputDatum'
  outputEpoch <- plet $ pfield @"currentEpoch" # outputDatum
  outputValue <- plet $ pfield @"value" # output
  
  txInfo <- plet $ getCtxInfoForSpending # ctx
  txRange <- plet $ pfield @"validRange" # txInfo
  now <- plet $ getLowerBoundTime # txRange
  calculatedEpoch <- plet $ posixToEpoch # now
  calculatedDayOfEpoch <- plet $ posixToDayOfEpoch # now

  popaque . unTermCont $ do
    checkNftIsInValue "1003" protocolCurrency stakingPoolThreadTokenName inputValue
    checkNftIsInValue "1004" protocolCurrency stakingPoolThreadTokenName outputValue
    pmatchC red >>= \case
      PDepositWithCurrentEpoch redData -> do
        pguardC "1103" (calculatedEpoch #== inputEpoch)
        depositAmt <- pletC $ pfromData $ pfield @"_0" # redData
        providerPkh <- pletC $ pfromData $ pfield @"_1" # redData
        checkDepositAmount depositAmt stPoolFields.govCurrency stPoolFields.govTokenName inputValue outputValue
        checkReceipt calculatedEpoch calculatedDayOfEpoch depositAmt stPoolFields.verTokenCurrency providerPkh txInfo 
        checkIsSignedBy "1109" providerPkh txInfo
        pguardC "1110" (dat #== outputDatum)
        validateStakingPoolInfoWithCurrentEpoch calculatedEpoch calculatedDayOfEpoch depositAmt stPoolFields.verTokenCurrency ctx
        pure $ pconstant ()

      POpenNewEpoch _ -> do
        pguardC "1113" (inputEpoch #< calculatedEpoch)
        pguardC "1114" (outputEpoch #== calculatedEpoch)
        protocolRefInputDatum <- pletC $ getProtocolDatumFromReferenceUtxo # stPoolFields.protocol # ctx
        managerPkh <- pletC $ extractPaymentPkhFromAddress #$ pfield @"managerAddress" # protocolRefInputDatum
        checkIsSignedBy "1115" managerPkh txInfo
        
        checkStakingPoolInfoWithNewEpoch calculatedEpoch stPoolFields.verTokenCurrency ctx
        pure $ pconstant ()

      _ -> pure $ ptraceError "not implemented"

checkDepositAmount :: 
  Term s PInteger ->
  Term s PCurrencySymbol -> 
  Term s PTokenName ->
  Term s SortedPositiveValue ->
  Term s SortedPositiveValue ->
  TermCont s ()
checkDepositAmount depositAmt govCurrency govTokenName inputValue outputValue = do
  pguardC "1104" (0 #< depositAmt)
  additionalValue <- pletC $ Value.psingleton # govCurrency # govTokenName # depositAmt
  expectedOutputValue <- pletC $ Value.pforgetPositive inputValue <> additionalValue
  pguardC "1105" (Value.pforgetPositive outputValue #== expectedOutputValue)

checkReceipt :: 
  Term s Epoch ->
  Term s DayOfEpoch ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s ProviderPkh ->
  Term s (PAsData PTxInfo) ->
  TermCont s ()
checkReceipt epoch dayOfEpoch depositAmount verTokenCurrency providerPkh txInfo = do
  receiptName <- pletC $ mkReceiptName # epoch # dayOfEpoch # depositAmount
  mintValue <- pletC $ Value.psingleton # verTokenCurrency # receiptName # 1
  checkExpectedMinting "1107" mintValue txInfo
  pguardC "1108" (pubKeyOutputContainsToken # providerPkh # verTokenCurrency # receiptName # 1 # txInfo)

validateStakingPoolInfoWithCurrentEpoch ::
  Term s Epoch ->
  Term s DayOfEpoch ->
  Term s DaoTokensAmt ->
  Term s PCurrencySymbol ->
  Term s PScriptContext ->
  TermCont s ()
validateStakingPoolInfoWithCurrentEpoch epoch dayOfEpoch daoTokensAmt verTokenCurrency ctx = do
  spiInput <- pletC $ getOnlyOneInputByToken # verTokenCurrency # stakingPoolVerTokenName # ctx
  spiInputDatum' <- pletC $ inlineDatumFromOutput # spiInput
  (spiInputDatum, _) <- ptryFromC @PStakingPoolInfoDatum spiInputDatum'
  spiInputDatumFields <- pletFieldsC @["epoch", "history"] spiInputDatum

  spiOutput <- pletC $ getOnlyOneOutputByToken # verTokenCurrency # stakingPoolVerTokenName # ctx
  spiOutputDatum' <- pletC $ inlineDatumFromOutput # spiOutput
  (spiOutputDatum, _) <- ptryFromC @PStakingPoolInfoDatum spiOutputDatum'
  spiOutputDatumFields <- pletFieldsC @["epoch", "history"] spiOutputDatum

  pguardC "1111" (spiInputDatumFields.epoch #== spiOutputDatumFields.epoch)
  updatedHistory <- pletC $ PMap.pinsert # dayOfEpoch # daoTokensAmt # spiInputDatumFields.history
  pguardC "1112" (spiOutputDatumFields.history #== updatedHistory)

checkStakingPoolInfoWithNewEpoch :: Term s Epoch -> Term s PCurrencySymbol -> Term s PScriptContext -> TermCont s ()
checkStakingPoolInfoWithNewEpoch newEpoch verTokenCurrency ctx = do
  spiOutput <- pletC $ getOnlyOneOutputByToken # verTokenCurrency # stakingPoolVerTokenName # ctx
  spiOutputDatum' <- pletC $ inlineDatumFromOutput # spiOutput
  (spiOutputDatum, _) <- ptryFromC @PStakingPoolInfoDatum spiOutputDatum'
  spiOutputDatumFields <- pletFieldsC @["epoch", "history"] spiOutputDatum
  pguardC "1117" (spiOutputDatumFields.epoch #== newEpoch)
  pguardC "1118" (spiOutputDatumFields.history #== PMap.pempty)
  
  txInfo <- pletC $ getCtxInfoForSpending # ctx
  mintValue <- pletC $ Value.psingleton # verTokenCurrency # stakingPoolVerTokenName # 1
  checkExpectedMinting "1116" mintValue txInfo
  outputValue <- pletC $ pfield @"value" # spiOutput
  expectedValue <- pletC $ minAdaValue <> mintValue
  pguardC "1121" (Value.pforgetPositive outputValue #== expectedValue)

tokenNameSizeLimit :: Term s PInteger
tokenNameSizeLimit = pconstant 32

mkReceiptName :: Term s (Epoch :--> DayOfEpoch :--> PInteger :--> PTokenName)
mkReceiptName  = phoistAcyclic $
  plam $ \epoch dayOfEpoch amount -> P.do
    prefix <- plet $ pencodeUtf8 # "DPStake"
    epochBS <- plet $ pencodeUtf8 # (pshow epoch)
    dayBs <- plet $ pencodeUtf8 # (pshow dayOfEpoch)
    amtBs <- plet $ pencodeUtf8 # (pshow amount)
    separator <- plet $ pconsBS # 46 # mempty -- 46 is ascii for "."
    tnBs <- plet $ prefix <> epochBS <> separator <> dayBs <> separator <> amtBs
    plet tnBs $ \tnBs ->
      pif
        (plengthBS # tnBs #<= tokenNameSizeLimit)
        (pcon . PTokenName $ tnBs)
        (ptraceError "1106")
