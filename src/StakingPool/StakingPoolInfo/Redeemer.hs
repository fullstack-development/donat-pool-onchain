module StakingPool.StakingPoolInfo.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Prelude

data PStakingPoolInfoRedeemer (s :: S)
  = PUpdateRecord (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PStakingPoolInfoRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PStakingPoolInfoRedeemer
