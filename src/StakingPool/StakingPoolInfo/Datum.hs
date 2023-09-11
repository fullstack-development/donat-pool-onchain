module StakingPool.StakingPoolInfo.Datum where

import Data.Typeable
import Ext.Plutarch.Extra.Time
import qualified GHC.Generics as GHC
import Plutarch.Api.V1
import qualified Plutarch.Api.V1.AssocMap as PMap
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class

type DaoTokensAmt = PInteger

data PStakingPoolInfoDatum (s :: S)
  = PStakingPoolInfoDatum
      ( Term
          s
          ( PDataRecord
              '[ "epoch" ':= Epoch
               , "history" ':= PMap.PMap 'PMap.Sorted DayOfEpoch DaoTokensAmt
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PStakingPoolInfoDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PStakingPoolInfoDatum
instance PTryFrom PData (PAsData PStakingPoolInfoDatum)
