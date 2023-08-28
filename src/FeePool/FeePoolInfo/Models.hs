module FeePool.FeePoolInfo.Models where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)
import Ply.Plutarch.Class
import Protocol.Model

data PFeePoolInfo (s :: S)
  = PFeePoolInfo
      ( Term
          s
          ( PDataRecord
              '[ "protocol" ':= PProtocol
               , "verTokenCurrency" ':= PCurrencySymbol
               , "verTokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFeePoolInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PFeePoolInfo)

data FeePoolInfo = FeePoolInfo
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , verTokenName :: TokenName
  }

type instance PlyArgOf PFeePoolInfo = FeePoolInfo
