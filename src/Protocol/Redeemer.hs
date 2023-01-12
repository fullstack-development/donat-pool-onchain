module Protocol.Redeemer where

import qualified GHC.Generics as GHC
import Plutarch.DataRepr
import Plutarch.Prelude
import Protocol.Datum

data PProtocolRedeemer (s :: S)
  = PUpdateProtocolConfig (Term s (PDataRecord '["_0" ':= PProtocolConfig]))
  | PCloseProtocol (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PProtocolRedeemer where
  type DPTStrat _ = PlutusTypeData
