module Governance.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

data PGovernanceDatum (s :: S)
  = PGovernanceDatum
      ( Term
          s
          ( PDataRecord
              '[ "quorum" ':= PInteger
               , "fee" ':= PInteger
               , "govCurrency" ':= PCurrencySymbol
               , "govTokenName" ':= PTokenName
               , "duration" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PGovernanceDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PGovernanceDatum
instance PTryFrom PData (PAsData PGovernanceDatum)
