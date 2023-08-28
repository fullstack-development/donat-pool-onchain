module FeePool.FeePoolInfo.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model

type DepositTime = PPOSIXTime
type DepositAmount = PInteger

data PFeePoolInfoRedeemer (s :: S)
  = PAddRecord (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFeePoolInfoRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFeePoolInfoRedeemer
