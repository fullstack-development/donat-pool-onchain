module Protocol.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

data PProtocolDatum (s :: S)
  = PProtocolDatum
      ( Term
          s
          ( PDataRecord
              '[ "minAmount" ':= PInteger
               , "maxAmount" ':= PInteger
               , "minDuration" ':= PInteger -- min duration in minutes
               , "maxDuration" ':= PInteger -- max duration in minutes
               , "protocolFee" ':= PInteger
               , "managerPkh" ':= PPubKeyHash
               , "tokenOriginRef" ':= PTxOutRef
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PProtocolDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolDatum
instance PTryFrom PData (PAsData PProtocolDatum)
