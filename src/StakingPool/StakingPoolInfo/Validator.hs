{-# LANGUAGE OverloadedRecordDot #-}

module StakingPool.StakingPoolInfo.Validator where

import MintingPolicy.VerToken (stakingPoolVerTokenName)
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Shared.Checks
import Shared.ScriptContextV2
import StakingPool.Models (PStakingPool, stakingPoolThreadTokenName)

stakingPoolInfoValidator :: ClosedTerm (PStakingPool :--> PValidator)
stakingPoolInfoValidator = plam $ \stakingPool _ _ ctx -> P.do
  infoFields <- pletFields @["protocol", "verTokenCurrency"] stakingPool
  protocolCurrency <- plet $ pfield @"protocolCurrency" # infoFields.protocol
  inputValue <- plet $ getOwnInputValue # ctx
  output <- plet $ getOnlyOneOwnOutput # ctx
  outputValue <- plet $ pfield @"value" # output
  popaque . unTermCont $ do 
    checkNftIsInTxInput protocolCurrency stakingPoolThreadTokenName ctx
    pguardC "1119" (inputValue #== outputValue)
    checkNftIsInValue "1120" infoFields.verTokenCurrency stakingPoolVerTokenName inputValue
    -- output datum new record check is inside StakingPool validator
    pure $ pconstant ()
