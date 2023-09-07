module FeePool.FeePoolInfo.Datum where

import Data.Typeable
import Ext.Plutarch.Extra.Time
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

type FeeAmount = PInteger

data PFeePoolInfoDatum (s :: S)
  = PFeePoolInfoDatum
      ( Term
          s
          ( PDataRecord
              '[ "epoch" ':= Epoch
               , "fee" ':= PMap.PMap 'PMap.Sorted DayOfEpoch FeeAmount
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PFeePoolInfoDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFeePoolInfoDatum
instance PTryFrom PData (PAsData PFeePoolInfoDatum)
