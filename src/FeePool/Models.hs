module FeePool.Models where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)
import Ply.Plutarch.Class
import Protocol.Model

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
  , verTokenName :: TokenName
  }

type instance PlyArgOf PFeePool = FeePool
