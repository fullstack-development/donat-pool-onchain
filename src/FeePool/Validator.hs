{-# LANGUAGE OverloadedRecordDot #-}

module FeePool.Validator where

import Ext.Plutarch.Extra.Time 
import Ext.Plutus.MinAda (minTxOut, minAdaValue)
import FeePool.Datum (PFeePoolDatum)
import FeePool.FeePoolInfo.Datum (FeeAmount, PFeePoolInfoDatum)
import FeePool.Models (PFeePool, feePoolThreadTokenName)
import FeePool.Redeemer (PFeePoolRedeemer(..))
import MintingPolicy.VerToken (feePoolVerTokenName)
import qualified Plutarch.Api.V1.Value as Value
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#+), (#-))
import Plutarch.Prelude
import Shared.Checks 
import Shared.ScriptContextV2

feePoolValidator :: ClosedTerm (PFeePool :--> PValidator)
feePoolValidator = plam $ \feePool dtm rdm ctx -> P.do
  (dat, _) <- ptryFrom @PFeePoolDatum dtm
  (red, _) <- ptryFrom @PFeePoolRedeemer rdm
  feePoolFields <- pletFields @["verTokenCurrency", "protocol"] feePool
  protocolToken <- pletFields @["protocolCurrency", "protocolTokenName"] feePoolFields.protocol
  inputEpoch <- plet $ pfield @"currentEpoch" # dat
  inputValue <- plet $ getOwnInputValue # ctx
  output <- plet $ getOnlyOneOwnOutput # ctx
  outputDatum' <- plet $ inlineDatumFromOutput # output
  (outputDatum, _) <- ptryFrom @PFeePoolDatum outputDatum'
  outputEpoch <- plet $ pfield @"currentEpoch" # outputDatum
  outputValue <- plet $ pfield @"value" # output
  
  txInfo <- plet $ getCtxInfoForSpending # ctx
  txRange <- plet $ pfield @"validRange" # txInfo

  popaque . unTermCont $ do
    checkNftIsInValue "1003" protocolToken.protocolCurrency feePoolThreadTokenName inputValue
    checkNftIsInValue "1004" protocolToken.protocolCurrency feePoolThreadTokenName outputValue
    pmatchC red >>= \case
      PAddFundsWithCurrentEpoch redData -> do
        pguardC "1005" (dat #== outputDatum)

        depositAmt <- pletC $ pfield @"_0" # redData  
        payment <- pletC $ Value.psingleton # Value.padaSymbol # Value.padaToken # depositAmt
        expectedOutputValue <- pletC $ (Value.pforgetPositive inputValue) <> payment
        pguardC "1006" (Value.pforgetPositive outputValue #== expectedOutputValue)
        pguardC "1015" (minTxOut #< depositAmt)

        receiveFundsTime <- pletC $ getLowerBoundTime # txRange
        calculatedEpoch <- pletC $ posixToEpoch # receiveFundsTime
        pguardC "1008" (calculatedEpoch #== inputEpoch)

        dayOfEpoch <- pletC $ posixToDayOfEpoch # receiveFundsTime
        validateFeePoolInfoWithCurrentEpoch calculatedEpoch dayOfEpoch depositAmt feePoolFields.verTokenCurrency ctx
        pure $ pconstant ()
      
      PAddFundsWithNewEpoch redData -> do
        depositAmt <- pletC $ pfield @"_0" # redData 
        
        receiveFundsTime <- pletC $ getLowerBoundTime # txRange
        calculatedEpoch <- pletC $ posixToEpoch # receiveFundsTime
        pguardC "1012" (calculatedEpoch #== outputEpoch)
        pguardC "1013" (pnot #$ calculatedEpoch #== inputEpoch)

        pguardC "1015" (minTxOut #< depositAmt)
        paymentToFeePool <- pletC $ Value.psingleton # Value.padaSymbol # Value.padaToken # depositAmt
        expectedOutputValue <- pletC $ (Value.pforgetPositive inputValue) <> paymentToFeePool
        pguardC "1016" (Value.pforgetPositive outputValue #== expectedOutputValue)
        
        dayOfEpoch <- pletC $ posixToDayOfEpoch # receiveFundsTime
        validateFeePoolInfoWithNewEpoch calculatedEpoch dayOfEpoch depositAmt feePoolFields.verTokenCurrency ctx
              
        pure $ pconstant ()
      PPayRewards _ -> pure $ ptraceError "not implemented"

validateFeePoolInfoWithCurrentEpoch :: 
  Term s Epoch ->
  Term s DayOfEpoch ->
  Term s FeeAmount ->
  Term s PCurrencySymbol -> 
  Term s PScriptContext ->
  TermCont s ()
validateFeePoolInfoWithCurrentEpoch epoch dayOfEpoch feeAmount verTokenCurrency ctx = do
  feePoolInfoInput <- pletC $ getOnlyOneInputByToken # verTokenCurrency # feePoolVerTokenName # ctx
  fpiInputDatum' <- pletC $ inlineDatumFromOutput # feePoolInfoInput
  (fpiInputDatum, _) <- ptryFromC @PFeePoolInfoDatum fpiInputDatum'
  fpiInputDatumFields <- pletFieldsC @["epoch", "fee"] fpiInputDatum

  feePoolInfoOutput <- pletC $ getOnlyOneOutputByToken # verTokenCurrency # feePoolVerTokenName # ctx
  fpiOutputDatum' <- pletC $ inlineDatumFromOutput # feePoolInfoOutput
  (fpiOutputDatum, _) <- ptryFromC @PFeePoolInfoDatum fpiOutputDatum'
  fpiOutputDatumFields <- pletFieldsC @["epoch", "fee"] fpiOutputDatum
  
  pguardC "1009" (fpiInputDatumFields.epoch #== fpiOutputDatumFields.epoch)
  mbCurrentFee <- pletC $ PMap.plookup # dayOfEpoch # fpiInputDatumFields.fee
  updatedFeeMap <- pletC $ pmatch mbCurrentFee $ \case 
      PNothing -> PMap.pinsert # dayOfEpoch # feeAmount # fpiInputDatumFields.fee
      PJust currentFee -> PMap.pinsert # dayOfEpoch # (feeAmount #+ currentFee) # fpiInputDatumFields.fee
  pguardC "1010" (fpiOutputDatumFields.fee #== updatedFeeMap)

validateFeePoolInfoWithNewEpoch :: 
  Term s Epoch ->
  Term s DayOfEpoch ->
  Term s FeeAmount ->
  Term s PCurrencySymbol -> 
  Term s PScriptContext ->
  TermCont s ()
validateFeePoolInfoWithNewEpoch epoch dayOfEpoch feeAmount verTokenCurrency ctx = do
  feePoolInfoOutput <- pletC $ getOnlyOneOutputByToken # verTokenCurrency # feePoolVerTokenName # ctx
  fpiOutputDatum' <- pletC $ inlineDatumFromOutput # feePoolInfoOutput
  (fpiOutputDatum, _) <- ptryFromC @PFeePoolInfoDatum fpiOutputDatum'
  fpiOutputDatumFields <- pletFieldsC @["epoch", "fee"] fpiOutputDatum

  pguardC "1017" (fpiOutputDatumFields.epoch #== epoch)
  expectedFeeMap <- pletC $ PMap.psingleton # dayOfEpoch # feeAmount
  pguardC "1018" (fpiOutputDatumFields.fee #== expectedFeeMap)
  outputValue <- pletC $ pfield @"value" # feePoolInfoOutput
  expectedOutputValue <- pletC $ Value.psingleton # verTokenCurrency # feePoolVerTokenName # 1 <> minAdaValue
  pguardC "1019" (Value.pforgetPositive outputValue #== expectedOutputValue)

  txInfo <- pletC $ getCtxInfoForSpending # ctx
  checkNftMinted "1020" 1 verTokenCurrency feePoolVerTokenName txInfo
