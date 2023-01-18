module Ext.Plutus.MinAda where

import Plutarch.Prelude

-- TODO: That should be configurable in future:
-- Read minUTxOValue from `testnet-shelley-genesis.json` cardano-node config
minTxOut :: Term s PInteger
minTxOut = 2_000_000
