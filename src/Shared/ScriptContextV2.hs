{-# LANGUAGE QualifiedDo #-}

module Shared.ScriptContextV2 where

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

type SortedPositiveValue = PValue 'Sorted 'Positive

inlineDatumFromOutput :: Term s (PScriptContext :--> PTxOut :--> PData)
inlineDatumFromOutput = phoistAcyclic $
  plam $ \ctx scriptTxOut ->
    pmatch (pfield @"datum" # scriptTxOut) $ \case
      PNoOutputDatum _ -> ptraceError "301"
      POutputDatumHash _ -> ptraceError "302"
      POutputDatum d ->
        pmatch (pfield @"outputDatum" # d) $ \case
          PDatum datum -> datum

getCtxInfoForSpending :: Term s (PScriptContext :--> PAsData PTxInfo)
getCtxInfoForSpending = phoistAcyclic $
  plam $ \ctx' ->
    pmatch (pfield @"purpose" #ctx') $ \case
      PSpending _ -> pfield @"txInfo" # ctx'
      _ -> ptraceError "303"

getOrefForSpending :: Term s (PScriptContext :--> PTxOutRef)
getOrefForSpending = phoistAcyclic $
  plam $ \ctx' ->
    pmatch (pfield @"purpose" #ctx') $ \case
      PSpending outRef -> pfield @"_0" # outRef
      _ -> ptraceError "304"

getOnlyOneOwnOutput :: Term s (PScriptContext :--> PTxOut)
getOnlyOneOwnOutput = phoistAcyclic $
  plam $ \ctx ->
    let ownOutputs = findOwnOutputs # ctx
     in getOnlyOneOutputFromList # ownOutputs

findOwnOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
findOwnOutputs = phoistAcyclic $
  plam $ \ctx' ->
    let outRef = getOrefForSpending # ctx'
        txInfo = pfield @"txInfo" # ctx'
        inputs = pfield @"inputs" # txInfo
        outputs = pfield @"outputs" # txInfo
     in pgetContinuingOutputs # inputs # outputs # outRef

findOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
findOwnInput = phoistAcyclic $
  plam $ \ctx' ->
    let outRef = getOrefForSpending # ctx'
        txInfo = pfield @"txInfo" # ctx'
        inputs = pfield @"inputs" # txInfo
     in pfindOwnInput # inputs # outRef

getOwnInputOrTraceError :: Term s (PScriptContext :--> PTxOut)
getOwnInputOrTraceError = phoistAcyclic $
  plam $ \ctx ->
    let mbTxInInfo = findOwnInput # ctx
        txInInfo = ptraceIfNothing "305" mbTxInInfo
     in pfield @"resolved" # txInInfo

getOwnInputValue :: Term s (PScriptContext :--> SortedPositiveValue)
getOwnInputValue = phoistAcyclic $
  plam $ \ctx ->
    let txOut = getOwnInputOrTraceError # ctx
     in pfield @"value" # txOut

pubKeyOutputsAt :: Term s (PPubKeyHash :--> PAsData PTxInfo :--> PTxOut)
pubKeyOutputsAt = phoistAcyclic $
  plam $ \pkh txInfo ->
    let outputs = pfield @"outputs" # txInfo
        pkhOutputs = pfilter # (matches # pkh) # outputs
     in getOnlyOneOutputFromList # pkhOutputs
  where
    matches :: Term s (PPubKeyHash :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \pkh txOut ->
        let adr = pfield @"address" # txOut
            credential = pfield @"credential" # adr
         in pmatch (pfromData credential) $ \case
              PPubKeyCredential pkh' -> (pfield @"_0" # pkh') #== pkh
              _ -> pconstant False

getOnlyOneOutputFromList :: Term s (PBuiltinList PTxOut :--> PTxOut)
getOnlyOneOutputFromList = phoistAcyclic $
  plam $ \outputs ->
    pmatch outputs $ \case
      PNil -> ptraceError "306"
      PCons scriptTxOut rest -> do
        pmatch rest $ \case
          PNil -> scriptTxOut
          _ -> ptraceError "307"
