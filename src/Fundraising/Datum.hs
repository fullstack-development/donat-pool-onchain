module Fundraising.Datum where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude

descStringSize :: Term s PInteger
descStringSize = pconstant 35

data PFundraisingDatum (s :: S)
  = PFundraisingDatum
      ( Term
          s
          ( PDataRecord
              '[ "creator" ':= PAddress
               , "tokenOrigin" ':= PTxOutRef
               , "frTitle" ':= PByteString -- description length is limited by descStringSize
               , "frAmount" ':= PInteger
               , "frDeadline" ':= PPOSIXTime
               , "frFee" ':= PInteger
               , "manager" ':= PAddress
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PFundraisingDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFundraisingDatum
instance PTryFrom PData (PAsData PFundraisingDatum)
