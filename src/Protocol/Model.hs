module Protocol.Model where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, TokenName)
import Ply.Plutarch.Class

data PProtocol (s :: S)
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
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PProtocol where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocol
instance PTryFrom PData (PAsData PProtocol)

protocolSymbol :: Term s PProtocol -> Term s PCurrencySymbol
protocolSymbol protocol = pfield @"protocolCurrency" # protocol

protocolToken :: Term s PProtocol -> Term s PTokenName
protocolToken protocol = pfield @"protocolTokenName" # protocol

data Protocol = Protocol
  { managerPkh :: PubKeyHash
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

type instance PlyArgOf PProtocol = Protocol

data PProtocolConfig (s :: S)
  = PProtocolConfig
      ( Term
          s
          ( PDataRecord
              '[ "minAmount" ':= PInteger
               , "maxAmount" ':= PInteger
               , "minDuration" ':= PInteger
               , "maxDuration" ':= PInteger
               , "protocolFee" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PProtocolConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProtocolConfig

instance PTryFrom PData (PAsData PProtocolConfig)

data PFundriseConfig (s :: S)
  = PFundriseConfig
      ( Term
          s
          ( PDataRecord
              '[ "scriptAddress" ':= PAddress
               , "verCurrencySymbol" ':= PCurrencySymbol
               , "verTokenName" ':= PTokenName
               , "threadCurrencySymbol" ':= PCurrencySymbol
               , "threadTokenName" ':= PTokenName
               , "startedAt" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFundriseConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFundriseConfig

instance PTryFrom PData (PAsData PFundriseConfig)
