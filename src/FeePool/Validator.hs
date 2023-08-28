{-# LANGUAGE OverloadedRecordDot #-}

module FeePool.Validator where

import Ext.Plutus.MinAda
import MintingPolicy.NFT (checkUTxOSpent)
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2
import Plutarch.Extra.Interval (pinterval)
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num (pnegate, (#+), (#-))
import Plutarch.Prelude
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus
import PlutusLedgerApi.V1 (PubKeyHash)
import Protocol.Datum (PProtocolDatum)
import Protocol.Model (PProtocolConfig, PProtocol, ProtocolConfig (..))
import Shared.Checks 
import Shared.ScriptContextV2

feePoolThreadTokenName :: Term s PTokenName
feePoolThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "DonatPoolFeePool")