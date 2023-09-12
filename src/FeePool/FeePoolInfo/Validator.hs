{-# LANGUAGE OverloadedRecordDot #-}

module FeePool.FeePoolInfo.Validator where

import FeePool.Models (PFeePool, feePoolThreadTokenName)
import MintingPolicy.VerToken (feePoolVerTokenName)
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Shared.Checks
import Shared.ScriptContextV2

feePoolInfoValidator :: ClosedTerm (PFeePool :--> PValidator)
feePoolInfoValidator = plam $ \feePool _ _ ctx -> P.do
  infoFields <- pletFields @["protocol", "verTokenCurrency"] feePool
  protocolCurrency <- plet $ pfield @"protocolCurrency" # infoFields.protocol
  inputValue <- plet $ getOwnInputValue # ctx
  output <- plet $ getOnlyOneOwnOutput # ctx
  outputValue <- plet $ pfield @"value" # output
  popaque . unTermCont $ do 
    checkNftIsInTxInput protocolCurrency feePoolThreadTokenName ctx
    pguardC "1001" (inputValue #== outputValue)
    checkNftIsInValue "1002" infoFields.verTokenCurrency feePoolVerTokenName inputValue
    -- output datum new record check is inside FeePool validator
    pure $ pconstant ()
