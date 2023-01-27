module Protocol.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

data PPoolSizeLimits (s :: S)
  = PPoolSizeLimits
      ( Term
          s
          ( PDataRecord
              '[ "minAmount" ':= PInteger
               , "maxAmount" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PPoolSizeLimits where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PPoolSizeLimits)
instance PTryFrom PData PPoolSizeLimits

data PDurationLimits (s :: S)
  = PDurationLimits
      ( Term
          s
          ( PDataRecord
              '[ "minDuration" ':= PInteger
               , "maxDuration" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PDurationLimits where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PDurationLimits)
instance PTryFrom PData PDurationLimits

data PProtocolConfig (s :: S)
  = PProtocolConfig
      ( Term
          s
          ( PDataRecord
              '[ "protocolFee" ':= PRational
               , "poolSizeLimits" ':= PPoolSizeLimits
               , "durationLimits" ':= PDurationLimits
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PProtocolConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolConfig

instance PTryFrom PData (PAsData PProtocolConfig)

data PProtocolConstants (s :: S)
  = PProtocolConstants
      ( Term
          s
          ( PDataRecord
              '[ "managerPkh" ':= PPubKeyHash
               , "tokenOriginRef" ':= PTxOutRef
               , "protocolCurrency" ':= PCurrencySymbol
               , "protocolTokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PProtocolConstants where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolConstants
instance PTryFrom PData (PAsData PProtocolConstants)

data PProtocolDatum (s :: S)
  = PProtocolDatum
      ( Term
          s
          ( PDataRecord
              '[ "protocolConstants" ':= PProtocolConstants
               , "protocolConfig" ':= PProtocolConfig
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PProtocolDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolDatum
instance PTryFrom PData (PAsData PProtocolDatum)
