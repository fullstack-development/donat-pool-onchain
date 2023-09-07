module FeePool.Redeemer where

import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model

type DepositTime = PPOSIXTime
type DepositAmount = PInteger

data PFeePoolRedeemer (s :: S)
  = PAddFundsWithCurrentEpoch (Term s (PDataRecord '["_0" ':= DepositAmount]))
  | PAddFundsWithNewEpoch (Term s (PDataRecord '["_0" ':= DepositAmount]))
  | PPayRewards (Term s (PDataRecord '[])) -- not implemented for now
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFeePoolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFeePoolRedeemer
