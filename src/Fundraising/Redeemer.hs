module Fundraising.Redeemer where 

import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude

-- Note:
-- _0 - Fundraising ThreadToken CurrencySymbol
-- _1 - Fundraising ThreadToken Token name
-- _2 - amount ot donate in Ada - must be greater than minAda
data PFundraisingRedeemer (s :: S)
  = PDonate (Term s (PDataRecord '[ "_0" ':= PCurrencySymbol, "_1" ':= PTokenName, "_2" ':= PInteger ]))
  | PReceiveFunds (Term s (PDataRecord '[ "_0" ':= PCurrencySymbol, "_1" ':= PTokenName ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFundraisingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFundraisingRedeemer
