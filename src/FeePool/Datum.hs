module FeePool.Datum where

import Data.Typeable
import Ext.Plutarch.Extra.Time
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

data PFeePoolDatum (s :: S)
  = PFeePoolDatum (Term s (PDataRecord '["currentEpoch" ':= Epoch]))
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PFeePoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFeePoolDatum
instance PTryFrom PData (PAsData PFeePoolDatum)
