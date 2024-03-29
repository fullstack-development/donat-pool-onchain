module Protocol.Redeemer where

import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum
import Protocol.Model

data PProtocolRedeemer (s :: S)
  = PUpdateProtocolConfig (Term s (PDataRecord '["_0" ':= PProtocolConfig]))
  | PStartFundrise (Term s (PDataRecord '["_0" ':= PFundriseConfig]))
  | PCloseProtocol (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PProtocolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolRedeemer
