module Fundraising.Datum where

import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude

data PFundraisingDatum (s :: S)
  = PFundraisingDatum
      ( Term
          s
          ( PDataRecord
              '[ "creatorPkh" ':= PPubKeyHash
               , "tokenOrigin" ':= PTxOutRef
               , "frDesc" ':= PByteString -- 35 symbols max
               , "frAmount" ':= PInteger
               , "frDeadline" ':= PPOSIXTime
               , "frFee" ':= PInteger
               , "frUrl" ':= PByteString -- 35 symbols max
               , "managerPkh" ':= PPubKeyHash
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PFundraisingDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFundraisingDatum
instance PTryFrom PData (PAsData PFundraisingDatum)
