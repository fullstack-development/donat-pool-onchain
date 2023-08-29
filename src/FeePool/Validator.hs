{-# LANGUAGE OverloadedRecordDot #-}

module FeePool.Validator where

import Ext.Plutarch.Extra.Time 
import Ext.Plutus.MinAda
import FeePool.Datum
import FeePool.FeePoolInfo.Datum
import FeePool.Models
import FeePool.Redeemer
import MintingPolicy.NFT (checkUTxOSpent)
import MintingPolicy.VerToken
import qualified Plutarch.Api.V1.Value as Value
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.Api.V2
import Plutarch.Extra.Interval (pinterval)
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num (pnegate, (#+), (#-))
import Plutarch.Prelude
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus
import PlutusLedgerApi.V1 (PubKeyHash)
import Protocol.Datum (PProtocolDatum)
import Protocol.Model (PProtocolConfig, PProtocol, ProtocolConfig (..))
import Shared.Checks 
import Shared.ScriptContextV2

feePoolThreadTokenName :: Term s PTokenName
feePoolThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolFeePool")

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
  outputValue <- plet $ pfield @"value" # output
  
  txInfo <- plet $ getCtxInfoForSpending # ctx
  txRange <- plet $ pfield @"validRange" # txInfo

  popaque . unTermCont $ do
    checkNftIsInValue "1003" protocolToken.protocolCurrency feePoolThreadTokenName inputValue
    checkNftIsInValue "1004" protocolToken.protocolCurrency feePoolThreadTokenName outputValue
    pmatchC red >>= \case
      PAddFundsWithCurrentEpoch redData -> do
        pguardC "1005" (dat #== outputDatum)

        redFields <- pletFieldsC @["_0", "_1"] redData  -- _0 - timestamp, _1 - ada amount        
        payment <- pletC $ Value.psingleton # Value.padaSymbol # Value.padaToken # redFields._1
        expectedOutputValue <- pletC $ (Value.pforgetPositive inputValue) <> payment
        pguardC "1006" (Value.pforgetPositive outputValue #== expectedOutputValue)

        receiveFundsTime <- pletC $ pfromData $ getLowerBoundTime # txRange
        pguardC "1007" (redFields._0 #== receiveFundsTime)

        calculatedEpoch <- pletC $ posixToEpoch # redFields._0
        pguardC "1008" (calculatedEpoch #== inputEpoch)

        dayOfEpoch <- pletC $ posixToDayOfEpoch # redFields._0
        validateFeePoolInfoWithCurrentEpoch calculatedEpoch dayOfEpoch (pfromData redFields._1) feePoolFields.verTokenCurrency ctx
        -- amount to deposit is checked in ReceiveFunds case (Fundraising validator)
        pure $ pconstant ()
      _ -> pure $ ptraceError "not implemented"

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
  updatedFeeMap <- pmatchC mbCurrentFee >>= \case 
      PNothing -> pletC $ PMap.pinsert # dayOfEpoch # feeAmount # fpiInputDatumFields.fee
      PJust currentFee -> pletC $ PMap.pinsert # dayOfEpoch # (feeAmount #+ currentFee) # fpiInputDatumFields.fee
  pguardC "1010" (fpiOutputDatumFields.fee #== updatedFeeMap)
