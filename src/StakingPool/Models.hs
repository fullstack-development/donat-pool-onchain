module StakingPool.Models where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import qualified PlutusLedgerApi.V1 as Plutus
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)
import qualified PlutusTx.Prelude as Plutus
import Ply.Plutarch.Class
import Protocol.Model

stakingPoolThreadTokenName :: Term s PTokenName
stakingPoolThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolStakingPool")

data PStakingPool (s :: S)
  = PStakingPool
      ( Term
          s
          ( PDataRecord
              '[ "protocol" ':= PProtocol
               , "verTokenCurrency" ':= PCurrencySymbol
               , "daoCurrency" ':= PCurrencySymbol
               , "daoTokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PStakingPool where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PStakingPool)

data StakingPool = StakingPool
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , daoCurrency :: CurrencySymbol
  , daoTokenName :: TokenName
  }

type instance PlyArgOf PStakingPool = StakingPool
