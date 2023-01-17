{-# LANGUAGE QualifiedDo #-}

module ScriptContext where

import Generics.SOP
import Plutarch.Api.V1
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Api
import Plutarch.Extra.Maybe
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1 (TxOut)
import Protocol.Datum
import Protocol.Redeemer

type SortedPositiveValue = PValue 'Sorted 'Positive

getCtxInfoForSpending :: Term s PScriptContext -> TermCont s (Term s (PAsData PTxInfo))
getCtxInfoForSpending ctx' = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- tcont . pmatch $ getField @"purpose" ctx
  pure $ getField @"txInfo" ctx

getOwnOutputDatum :: Term s PScriptContext -> TermCont s (Term s (PAsData PProtocolDatum))
getOwnOutputDatum ctx = getOnlyOneOwnOutput ctx >>= getOwnOutputDatumFromTxOut ctx

getOwnOutputDatumFromTxOut :: Term s PScriptContext -> Term s PTxOut -> TermCont s (Term s (PAsData PProtocolDatum))
getOwnOutputDatumFromTxOut ctx txOut = do
  datum <- datumFromOutput ctx txOut
  pmatchC datum >>= \case
    PJust d -> pure d
    PNothing -> pure $ ptraceError "can't get datum by datum hash"

datumFromOutput :: Term s PScriptContext -> Term s PTxOut -> TermCont s (Term s (PMaybe (PAsData PProtocolDatum)))
datumFromOutput ctx scriptTxOut = do
  pmatchC (pfield @"datumHash" # scriptTxOut) >>= \case
    PDJust datHash' -> do
      let datumHash = pfield @"_0" # datHash'
      ctxInfo' <- getCtxInfoForSpending ctx
      let d = pparseDatum @PProtocolDatum # datumHash #$ pfield @"datums" # ctxInfo'
      pure d
    PDNothing _ -> pure $ ptraceError "no datum hash in script output"

getOnlyOneOwnOutput :: Term s PScriptContext -> TermCont s (Term s PTxOut)
getOnlyOneOwnOutput ctx = do
  outputs <- findOwnOutputs ctx
  pmatchC outputs >>= \case
    PNil -> pure $ ptraceError "no outputs found in context"
    PCons scriptTxOut rest -> do
      pmatchC rest >>= \case
        PNil -> pure scriptTxOut
        _ -> pure $ ptraceError "more then 1 outputs found in context"

findOwnOutputs :: Term s PScriptContext -> TermCont s (Term s (PBuiltinList PTxOut))
findOwnOutputs ctx = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # getField @"txInfo" ctx
          outputs = pfield @"outputs" # getField @"txInfo" ctx
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ -> pure $ ptraceError "not a spending tx"

findOwnInput :: Term s PScriptContext -> TermCont s (Term s (PMaybe PTxInInfo))
findOwnInput ctx' = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # getField @"txInfo" ctx
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"

getOwnInputOrTraceError :: Term s PScriptContext -> TermCont s (Term s PTxOut)
getOwnInputOrTraceError ctx = do
  mbTxInInfo <- findOwnInput ctx
  let txInInfo = pfromMaybe # ptraceError "not a spending tx" # mbTxInInfo
  pure $ pfield @"resolved" # txInInfo

getOwnInputValue :: Term s PScriptContext -> TermCont s (Term s SortedPositiveValue)
getOwnInputValue ctx = do
  txOut <- getOwnInputOrTraceError ctx
  pure $ getOwnOutputValueFromTxOut txOut

getOwnOutValue :: Term s PScriptContext -> TermCont s (Term s SortedPositiveValue)
getOwnOutValue ctx = do
  txOut <- getOnlyOneOwnOutput ctx
  pure $ getOwnOutputValueFromTxOut txOut

getOwnOutputValueFromTxOut :: Term s PTxOut -> Term s SortedPositiveValue
getOwnOutputValueFromTxOut txOut = pfield @"value" # txOut
