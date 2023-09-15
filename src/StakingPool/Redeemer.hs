module StakingPool.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import StakingPool.StakingPoolInfo.Datum (DaoTokensAmt)

type ProviderPkh = PPubKeyHash

data PStakingPoolRedeemer (s :: S)
  = PDepositWithCurrentEpoch (Term s (PDataRecord '["_0" ':= DaoTokensAmt, "_1" ':= ProviderPkh]))
  | POpenNewEpoch (Term s (PDataRecord '[]))
  | PWithdrawRewards (Term s (PDataRecord '[])) -- not implemented for now
  | PWithdrawInFull (Term s (PDataRecord '[])) -- not implemented for now
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PStakingPoolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PStakingPoolRedeemer
