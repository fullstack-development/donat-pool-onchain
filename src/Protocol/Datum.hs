module Protocol.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

-- TODO: move to model
newtype PProtocol (s :: S)
  = PProtocol
      ( Term
          s
          ( PDataRecord
              '[ "managerPkh" ':= PPubKeyHash
               , "protocolCurrency" ':= PCurrencySymbol
               , "protocolTokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, Typeable)

instance DerivePlutusType PProtocol where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PProtocol)

protocolSymbol :: Term s PProtocol -> Term s PCurrencySymbol
protocolSymbol protocol = pfield @"protocolCurrency" # protocol

protocolToken :: Term s PProtocol -> Term s PTokenName
protocolToken protocol = pfield @"protocolTokenName" # protocol

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PPoolSizeLimits where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PPoolSizeLimits)

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PDurationLimits where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PDurationLimits)

newtype PProtocolConfig (s :: S)
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
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PProtocolConfig)

newtype ProtocolConstants (s :: S)
  = ProtocolConstants
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

instance DerivePlutusType ProtocolConstants where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData ProtocolConstants
instance PTryFrom PData (PAsData ProtocolConstants)

newtype PProtocolDatum (s :: S)
  = PProtocolDatum
      ( Term
          s
          ( PDataRecord
              '[ "protocolConstants" ':= ProtocolConstants
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
