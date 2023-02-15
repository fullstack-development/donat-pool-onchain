module Fundraising.Redeemer where

import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude

-- Note:
-- _0 - amount ot donate in Ada - must be greater than minAda
-- _1 - donatedAt
data PFundraisingRedeemer (s :: S)
  = PDonate (Term s (PDataRecord '["_0" ':= PInteger]))
  | PReceiveFunds (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFundraisingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFundraisingRedeemer
