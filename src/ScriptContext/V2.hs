{-# LANGUAGE QualifiedDo #-}

module ScriptContext.V2 where

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

getCtxInfoForSpending :: Term s PScriptContext -> TermCont s (Term s (PAsData PTxInfo))
getCtxInfoForSpending ctx' = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- tcont . pmatch $ getField @"purpose" ctx
  pure $ getField @"txInfo" ctx

getOwnOutputInlineDatum :: Term s PScriptContext -> TermCont s (Term s PData)
getOwnOutputInlineDatum ctx = getOnlyOneOwnOutput ctx >>= inlineDatumFromOutput ctx

inlineDatumFromOutput :: Term s PScriptContext -> Term s PTxOut -> TermCont s (Term s PData)
inlineDatumFromOutput ctx scriptTxOut = do
  pmatchC (pfield @"datum" # scriptTxOut) >>= \case
    PNoOutputDatum _ -> pure $ ptraceError "301"
    POutputDatumHash _ -> pure $ ptraceError "302"
    POutputDatum d -> do
      let datum' = pfield @"outputDatum" # d
      PDatum datum <- pmatchC datum'
      pure datum

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
    _ -> pure $ ptraceError "303"

findOwnInput :: Term s PScriptContext -> TermCont s (Term s (PMaybe PTxInInfo))
findOwnInput ctx' = do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # getField @"txInfo" ctx
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceError "304"

getOwnInputOrTraceError :: Term s PScriptContext -> TermCont s (Term s PTxOut)
getOwnInputOrTraceError ctx = do
  mbTxInInfo <- findOwnInput ctx
  let txInInfo = ptraceIfNothing "305" mbTxInInfo
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
  pguardC "306" $ mintingTokenAmount #== -1
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
  pguardC "307" $ minAdaAmount #== expectedValue
  pure ()

getOnlyOneOutputFromList :: Term s (PBuiltinList PTxOut) -> TermCont s (Term s PTxOut)
getOnlyOneOutputFromList outputs = do
  pmatchC outputs >>= \case
    PNil -> pure $ ptraceError "308"
    PCons scriptTxOut rest -> do
      pmatchC rest >>= \case
        PNil -> pure scriptTxOut
        _ -> pure $ ptraceError "309"

checkNoOutputs :: Term s PScriptContext -> TermCont s (Term s PUnit)
checkNoOutputs ctx = do
  outputs <- findOwnOutputs ctx
  pmatchC outputs >>= \case
    PNil -> pure $ pconstant ()
    PCons _ _ -> pure $ ptraceError "310"

checkMintingAmount :: Term s PInteger -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkMintingAmount amt tn ctx' = do 
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["mint"] $ getField @"txInfo" ctx
    pguardC "Wrong NFT mint amount" $
      pvalueOf # getField @"mint" txInfo # ownSym # tn #== amt
