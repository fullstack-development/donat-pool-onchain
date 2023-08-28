{-# LANGUAGE OverloadedRecordDot #-}

module FeePool.FeePoolInfo.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import FeePool.FeePoolInfo.Models
import FeePool.Validator (feePoolThreadTokenName)
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Protocol.Model
import Shared.Checks
import Shared.ScriptContextV2

feePoolInfoValidator :: ClosedTerm (PFeePoolInfo :--> PValidator)
feePoolInfoValidator = plam $ \feePoolInfo _ _ ctx -> P.do
  infoFields <- pletFields @["protocol", "verTokenCurrency", "verTokenName"] feePoolInfo
  protocolCurrency <- plet $ pfield @"protocolCurrency" # infoFields.protocol
  inputValue <- plet $ getOwnInputValue # ctx
  output <- plet $ getOnlyOneOwnOutput # ctx
  outputValue <- plet $ pfield @"value" # output
  popaque . unTermCont $ do 
    checkNftIsInTxInput protocolCurrency feePoolThreadTokenName ctx
    pguardC "1001" (inputValue #== outputValue)
    checkNftIsInValue "1002" infoFields.verTokenCurrency infoFields.verTokenName inputValue
    -- output datum new record check is inside FeePool validator
    pure $ pconstant ()
