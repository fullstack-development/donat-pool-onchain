module Fundraising.Model where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V2 (CurrencySymbol, PubKeyHash, TokenName)
import Ply.Plutarch.Class
import Protocol.Model

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
