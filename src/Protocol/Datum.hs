module Protocol.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

-- data PProtocolConfig (s :: S)
--   = PProtocolConfig
--       ( Term
--           s
--           ( PDataRecord
--               '[ "minAmount" ':= PInteger
--                , "maxAmount" ':= PInteger
--                , "minDuration" ':= PInteger
--                , "maxDuration" ':= PInteger
--                , "protocolFee" ':= PRational
--                ]
--           )
--       )
--   deriving stock (GHC.Generic)
--   deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

-- instance DerivePlutusType PProtocolConfig where
--   type DPTStrat _ = PlutusTypeData

-- instance PTryFrom PData PProtocolConfig

-- instance PTryFrom PData (PAsData PProtocolConfig)

data PProtocolDatum (s :: S)
  = PProtocolDatum
      ( Term
          s
          ( PDataRecord
              '[ "minAmount" ':= PInteger
               , "maxAmount" ':= PInteger
               , "minDuration" ':= PInteger
               , "maxDuration" ':= PInteger
               , "protocolFee" ':= PRational
               , "managerPkh" ':= PPubKeyHash
               , "tokenOriginRef" ':= PTxOutRef
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PProtocolDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolDatum
instance PTryFrom PData (PAsData PProtocolDatum)
