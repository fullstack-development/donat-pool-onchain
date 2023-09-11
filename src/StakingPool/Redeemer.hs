module StakingPool.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model

type DaoTokensAmt = PInteger

data PStakingPoolRedeemer (s :: S)
  = PDepositWithCurrentEpoch (Term s (PDataRecord '["_0" ':= DaoTokensAmt]))
  | PDepositWithNewEpoch (Term s (PDataRecord '["_0" ':= DaoTokensAmt]))
  | PWithdrawRewards (Term s (PDataRecord '[])) -- not implemented for now
  | PWithdrawFull (Term s (PDataRecord '[])) -- not implemented for now
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PStakingPoolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PStakingPoolRedeemer
