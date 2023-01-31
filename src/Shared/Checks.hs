{-# LANGUAGE QualifiedDo #-}

module Shared.Checks where

import Ext.Plutarch.Extra.ApiV2
import Generics.SOP
import Plutarch.Api.V1.Address
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Maybe
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V2 (PubKeyHash, TxOut)
import Protocol.Datum
import Protocol.Redeemer
import Shared.ScriptContextV2

checkNoOutputs :: Term s PScriptContext -> TermCont s (Term s PUnit)
checkNoOutputs ctx = do
  let outputs = findOwnOutputs # ctx
  pmatchC outputs >>= \case
    PNil -> pure $ pconstant ()
    PCons _ _ -> pure $ ptraceError "201"

checkNftBurnt :: Term s PCurrencySymbol -> Term s PTokenName -> Term s (PAsData PTxInfo) -> TermCont s ()
checkNftBurnt currency tokenName txInfo = do
  let mintValue = pfield @"mint" # txInfo
      mintingTokenAmount = pvalueOf # mintValue # currency # tokenName
  pguardC "202" $ mintingTokenAmount #== -1
  pure ()

checkPkhReceiveScriptValue :: Term s PPubKeyHash -> Term s PInteger -> Term s (PAsData PTxInfo) -> TermCont s ()
checkPkhReceiveScriptValue pkh expectedValue txOut = do
  let pkhOutput = pubKeyOutputsAt # pkh # txOut
      pkhOutputValue = pfield @"value" # pkhOutput
      minAdaAmount = pvalueOf # pkhOutputValue # padaSymbol # padaToken
  pguardC "203" $ minAdaAmount #== expectedValue
  pure ()

checkMintingAmount :: Term s PInteger -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkMintingAmount amt tn ctx' = do 
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["mint"] $ getField @"txInfo" ctx
    pguardC "Wrong NFT mint amount" $
      pvalueOf # getField @"mint" txInfo # ownSym # tn #== amt
