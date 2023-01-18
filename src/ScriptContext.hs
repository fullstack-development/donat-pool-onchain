{-# LANGUAGE QualifiedDo #-}

module ScriptContext where

import Generics.SOP
import Plutarch.Api.V1
import Plutarch.Api.V1.Value
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Api
import Plutarch.Extra.Maybe
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import PlutusLedgerApi.V1 (PubKeyHash, TxOut)
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
      txInfo' <- getCtxInfoForSpending ctx
      let d = pparseDatum @PProtocolDatum # datumHash #$ pfield @"datums" # txInfo'
      pure d
    PDNothing _ -> pure $ ptraceError "no datum hash in script output"

getOnlyOneOwnOutput :: Term s PScriptContext -> TermCont s (Term s PTxOut)
getOnlyOneOwnOutput ctx = findOwnOutputs ctx >>= getOnlyOneOutputFromList

findOwnOutputs :: Term s PScriptContext -> TermCont s (Term s (PBuiltinList PTxOut))
findOwnOutputs ctx' = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
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

checkNftBurnt :: Term s PCurrencySymbol -> Term s PTokenName -> Term s (PAsData PTxInfo) -> TermCont s ()
checkNftBurnt currency tokenName txInfo = do
  let mintValue = pfield @"mint" # txInfo
  let mintingTokenAmount = pvalueOf # mintValue # currency # tokenName
  pguardC "should burn 1 thread token" $ mintingTokenAmount #== -1
  pure ()

pubKeyOutputsAt :: Term s PPubKeyHash -> Term s (PAsData PTxInfo) -> TermCont s (Term s PTxOut)
pubKeyOutputsAt pkh txInfo = do
  let outputs = pfield @"outputs" # txInfo
  let pkhOutputs = pfilter # (matches # pkh) # outputs
  getOnlyOneOutputFromList pkhOutputs
  where
    matches :: Term s (PPubKeyHash :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \pkh txOut ->
        let adr = pfield @"address" # txOut
            credential = pfield @"credential" # adr
         in pmatch (pfromData credential) $ \case
              PPubKeyCredential pkh' -> (pfield @"_0" # pkh') #== pkh
              _ -> pconstant False

checkPkhReceiveScriptValue :: Term s PPubKeyHash -> Term s PInteger -> Term s (PAsData PTxInfo) -> TermCont s ()
checkPkhReceiveScriptValue pkh expectedValue txOut = do
  pkhOutput <- pubKeyOutputsAt pkh txOut
  let pkhOutputValue = getOwnOutputValueFromTxOut pkhOutput
  let minAdaAmount = pvalueOf # pkhOutputValue # padaSymbol # padaToken
  pguardC "should be minAda in pkh output" $ minAdaAmount #== expectedValue
  pure ()

getOnlyOneOutputFromList :: Term s (PBuiltinList PTxOut) -> TermCont s (Term s PTxOut)
getOnlyOneOutputFromList outputs = do
  pmatchC outputs >>= \case
    PNil -> pure $ ptraceError "no outputs found in context, expected 1"
    PCons scriptTxOut rest -> do
      pmatchC rest >>= \case
        PNil -> pure scriptTxOut
        _ -> pure $ ptraceError "many outputs found in context, expected 1"

checkNoOutputs :: Term s PScriptContext -> TermCont s (Term s PUnit)
checkNoOutputs ctx = do
  outputs <- findOwnOutputs ctx
  pmatchC outputs >>= \case
    PNil -> pure $ pconstant ()
    PCons _ _ -> pure $ ptraceError "more then 1 outputs found in context, expected 0"
