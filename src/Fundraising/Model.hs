module Fundraising.Model where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, TokenName)
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
               , "threadTokenCurrency" ':= PCurrencySymbol
               , "threadTokenName" ':= PTokenName
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

threadTokenSymbol :: Term s PFundraising -> Term s PCurrencySymbol
threadTokenSymbol fundraising = pfield @"threadTokenCurrency" # fundraising

threadToken :: Term s PFundraising -> Term s PTokenName
threadToken fundraising = pfield @"threadTokenName" # fundraising

data Fundraising = Fundraising
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , verTokenName :: TokenName
  , threadTokenCurrency :: CurrencySymbol
  , threadTokenName :: TokenName
  }

type instance PlyArgOf PFundraising = Fundraising
