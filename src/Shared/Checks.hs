{-# LANGUAGE QualifiedDo #-}

module Shared.Checks where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutarch.Extra.Time
import Generics.SOP
import Plutarch.Api.V1.Address
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Interval
import Plutarch.Extra.Maybe
import Plutarch.Extra.TermCont
import qualified Plutarch.List as List
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

checkNftMinted ::
  Term s PString ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PAsData PTxInfo) ->
  TermCont s ()
checkNftMinted errMsg amount currency tokenName txInfo = do
  let mintValue = pfield @"mint" # txInfo
      mintingTokenAmount = pvalueOf # mintValue # currency # tokenName
  pguardC errMsg $ mintingTokenAmount #== amount
  pure ()

checkMintingAmount :: Term s PInteger -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkMintingAmount amt tn ctx' = do
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
  let ownSym = pfield @"_0" # mintFlds
  txInfo <- tcont $ pletFields @'["mint"] $ getField @"txInfo" ctx
  pguardC "204" $
    pvalueOf # getField @"mint" txInfo # ownSym # tn #== amt

checkNftIsInTxInput :: Term s PCurrencySymbol -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkNftIsInTxInput cs tn ctx = do
  let inputs = getAllTxInputs # ctx
  checkNftIsInTxOutList "205" cs tn inputs
  pure ()

checkNftIsInTxOutput :: Term s PCurrencySymbol -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkNftIsInTxOutput cs tn ctx = do
  let outputs = getAllTxOutputs # ctx
  checkNftIsInTxOutList "206" cs tn outputs
  pure ()

checkNftIsInTxOutList :: Term s PString -> Term s PCurrencySymbol -> Term s PTokenName -> Term s (PBuiltinList PTxOut) -> TermCont s ()
checkNftIsInTxOutList errMsg cs tn txOuts = do
  pguardC errMsg $
    List.pany
      # plam
        ( \out ->
            let inputValue = pfield @"value" # out
             in pvalueOf # inputValue # cs # tn #== 1
        )
      # txOuts

checkNftIsInValue :: Term s PString -> Term s PCurrencySymbol -> Term s PTokenName -> Term s SortedPositiveValue -> TermCont s ()
checkNftIsInValue errMsg cs tn val = do
  let nftAmt = pvalueOf # val # cs # tn
  pguardC errMsg (nftAmt #== 1)

checkIsSignedBy :: Term s PString -> Term s PPubKeyHash -> Term s (PAsData PTxInfo) -> TermCont s ()
checkIsSignedBy errMsg pkh txInfo = do
  let signatories = pfield @"signatories" # txInfo
  pguardC errMsg $ pelem # pdata pkh # pfromData signatories

checkPermittedDuration ::
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PPOSIXTime) ->
  Term s (PAsData PPOSIXTime) ->
  TermCont s ()
checkPermittedDuration minDurationMinutes maxDurationMinutes startedAt deadline = do
  let minDuration = minutesToPosixDuration # minDurationMinutes # startedAt
  let maxDuration = minutesToPosixDuration # maxDurationMinutes # startedAt
  let permittedDuration = pinterval # minDuration # maxDuration
  pguardC "126" (pmember # deadline # permittedDuration)
