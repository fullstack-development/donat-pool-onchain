{-# LANGUAGE QualifiedDo #-}

module Protocol.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import Generics.SOP
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Datum
import Protocol.Model
import Protocol.Redeemer
import Shared.Checks
import Shared.ScriptContextV2

-- TODO: Add config values validation for updateProtocol
-- validate config values:
-- -- minPoolSize >= minAdaTxOutAmount
-- -- maxPoolSize ?
-- -- minDuration >= 1 day
-- -- maxDuration <= 90 days
-- -- fee > 0 and < 100 %

protocolValidator :: ClosedTerm (PProtocol :--> PValidator)
protocolValidator = plam $ \protocol datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PProtocolDatum datm
  (red, _) <- ptryFrom @PProtocolRedeemer redm
  pmatch red $ \case
    PUpdateProtocolConfig redData -> popaque . unTermCont $ do
      checkSignedByManager protocol ctx
      checkUpdateProtocolOutput protocol dat ctx
      pure $ pconstant ()
    PCloseProtocol _ -> popaque . unTermCont $ do
      let txInfo = getCtxInfoForSpending # ctx
      checkNoOutputs ctx
      checkNftBurned protocol txInfo
      pure $ pconstant ()

checkSignedByManager :: Term s PProtocol -> Term s PScriptContext -> TermCont s ()
checkSignedByManager protocol ctx' = do
  let txInfo = getCtxInfoForSpending # ctx'
  let signatories = pfield @"signatories" # txInfo
  let managerPkh = pfield @"managerPkh" # protocol
  let present = pelem # pdata managerPkh # pfromData signatories
  pguardC "111" $ pelem # pdata managerPkh # pfromData signatories

checkUpdateProtocolOutput :: Term s PProtocol -> Term s PProtocolDatum -> Term s PScriptContext -> TermCont s ()
checkUpdateProtocolOutput protocol inDatum ctx = do
  let output = getOnlyOneOwnOutput # ctx
  checkUpdateProtocolDatum inDatum ctx output
  checkUpdateProtocolValue protocol ctx output
  pure ()

checkUpdateProtocolDatum :: Term s PProtocolDatum -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolDatum inDatum ctx txOut = do
  let outDatum' = inlineDatumFromOutput # ctx # txOut
  (outDatum, _) <- ptryFromC @PProtocolDatum outDatum'
  pguardC "112" (pfield @"managerPkh" # inDatum #== pfield @"managerPkh" # outDatum)
  pguardC "113" (pfield @"tokenOriginRef" # inDatum #== pfield @"tokenOriginRef" # outDatum)
  pguardC "114" (pnot #$ inDatum #== outDatum)
  pure ()

checkUpdateProtocolValue :: Term s PProtocol -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolValue protocol ctx txOut = do
  let inValue = getOwnInputValue # ctx
  let outValue = pfield @"value" # txOut
  pguardC "115" (inValue #== outValue)
  let threadTokenAmount = pvalueOf # outValue # protocolSymbol protocol # protocolToken protocol
  pguardC "116" (threadTokenAmount #== 1)
  pure ()

checkNftBurned :: Term s PProtocol -> Term s (PAsData PTxInfo) -> TermCont s ()
checkNftBurned protocol = checkNftBurnt (protocolSymbol protocol) (protocolToken protocol)
