module Ext.Plutus.MinAda where

import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2 (KeyGuarantees(..), AmountGuarantees(..), PValue)
import Plutarch.Prelude

-- TODO: That should be configurable in future:
-- Read minUTxOValue from `testnet-shelley-genesis.json` cardano-node config
minTxOut :: Term s PInteger
minTxOut = 2_000_000

minAdaValue :: Term s (PValue 'Sorted 'NonZero)
minAdaValue = Value.psingleton # Value.padaSymbol # Value.padaToken # minTxOut
