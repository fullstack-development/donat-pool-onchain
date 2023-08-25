module FeePool.FeePoolInfo.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model

type DepositTime = PPOSIXTime
type DepositAmount = PInteger

data PFeePoolRedeemer (s :: S)
  = PAddRecord (Term s (PDataRecord '["_0" ':= DepositTime, "_1" ':= DepositAmount]))
  | PStartNewEpoch (Term s (PDataRecord '["_0" ':= DepositTime, "_1" ':= DepositAmount]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFeePoolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFeePoolRedeemer
