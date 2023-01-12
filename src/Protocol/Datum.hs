module Protocol.Datum where

import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude

newtype PPoolSizeLimits (s :: S)
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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PPoolSizeLimits where
  type DPTStrat _ = PlutusTypeNewtype

newtype PDurationLimits (s :: S)
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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PDurationLimits where
  type DPTStrat _ = PlutusTypeNewtype

newtype PProtocolConfig (s :: S)
  = PProtocolConfig
      ( Term
          s
          ( PDataRecord
              '[ "protocolFee" ':= PInteger
               , "poolSizeLimits" ':= PPoolSizeLimits
               , "durationLimits" ':= PDurationLimits
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PProtocolConfig where
  type DPTStrat _ = PlutusTypeNewtype

newtype PProtocolDatum (s :: S)
  = PProtocolDatum
      ( Term
          s
          ( PDataRecord
              '[ "managerPkh" ':= PPubKeyHash
               , "tokenOriginRef" ':= PTxOutRef
               , "protocolCurrency" ':= PCurrencySymbol
               , "protocolTokenName" ':= PTokenName
               , "protocolConfig" ':= PProtocolConfig
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PProtocolDatum where
  type DPTStrat _ = PlutusTypeNewtype
