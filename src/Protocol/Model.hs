module Protocol.Model where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, Typeable)

instance DerivePlutusType PProtocol where
  type DPTStrat _ = PlutusTypeData

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
