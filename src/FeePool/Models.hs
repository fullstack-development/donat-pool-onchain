module FeePool.Models where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import qualified PlutusLedgerApi.V1 as Plutus
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)
import qualified PlutusTx.Prelude as Plutus
import Ply.Plutarch.Class
import Protocol.Model

feePoolThreadTokenName :: Term s PTokenName
feePoolThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolFeePool")

data PFeePool (s :: S)
  = PFeePool
      ( Term
          s
          ( PDataRecord
              '[ "protocol" ':= PProtocol
               , "verTokenCurrency" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFeePool where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PFeePool)

data FeePool = FeePool
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  }

type instance PlyArgOf PFeePool = FeePool
