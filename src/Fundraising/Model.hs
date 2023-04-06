module Fundraising.Model where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, TokenName)
import Ply.Plutarch.Class
import Protocol.Model

byteStringSize :: Term s PInteger
byteStringSize = pconstant 35

data PFundraising (s :: S)
  = PFundraising
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

instance DerivePlutusType PFundraising where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PFundraising)

verTokenSymbol :: Term s PFundraising -> Term s PCurrencySymbol
verTokenSymbol fundraising = pfield @"verTokenCurrency" # fundraising

verToken :: Term s PFundraising -> Term s PTokenName
verToken fundraising = pfield @"verTokenName" # fundraising

data Fundraising = Fundraising
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , verTokenName :: TokenName
  }

type instance PlyArgOf PFundraising = Fundraising
