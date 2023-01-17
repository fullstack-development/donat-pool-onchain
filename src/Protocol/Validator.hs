{-# LANGUAGE QualifiedDo #-}

module Protocol.Validator where

import Generics.SOP
import Plutarch.Api.V1
import Plutarch.Api.V1.Value
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Api
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Datum
import Protocol.Redeemer
import ScriptContext

-- validate config values:
-- -- minPoolSize >= minAdaTxOutAmount
-- -- maxPoolSize ?
-- -- minDuration >= 1 day
-- -- maxDuration <= 90 days
-- -- fee > 0 and < 100 %

-- {-# INLINEABLE validateStabilityFee #-}
-- validateStabilityFee :: Rational -> Bool
-- validateStabilityFee stabilityFee = stabilityFee >= Ratio.fromInteger 0 && stabilityFee < Ratio.fromInteger 1

protocolValidator :: ClosedTerm (PProtocol :--> PValidator)
protocolValidator = plam $ \protocol datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PProtocolDatum datm
  (red, _) <- ptryFrom @PProtocolRedeemer redm
  pmatch red $ \case
    PUpdateProtocolConfig redData -> popaque . unTermCont $ do
      let newConf = pfield @"_0" # redData
      checkSignedByManager protocol ctx
      checkConfigChanged dat newConf
      checkUpdateProtocolOutput protocol dat newConf ctx
      pure $ pconstant ()
    PCloseProtocol _ -> popaque . unTermCont $ do
      pure $ pconstant () -- TODO

checkSignedByManager :: Term s PProtocol -> Term s PScriptContext -> TermCont s ()
checkSignedByManager protocol ctx' = do
  ctxInfo <- getCtxInfoForSpending ctx'
  let signatories = pfield @"signatories" # ctxInfo
  let managerPkg = pfield @"managerPkh" # protocol
  let present = pelem # pdata managerPkg # pfromData signatories
  pguardC "Wrong pkh" $ pelem # pdata managerPkg # pfromData signatories

checkConfigChanged :: Term s PProtocolDatum -> Term s PProtocolConfig -> TermCont s ()
checkConfigChanged datm newConf = P.do
  let oldConfig = pfield @"protocolConfig" # datm
  pguardC "132" (pnot #$ oldConfig #== newConf)

checkUpdateProtocolOutput :: Term s PProtocol -> Term s PProtocolDatum -> Term s PProtocolConfig -> Term s PScriptContext -> TermCont s ()
checkUpdateProtocolOutput protocol inDatum newConf ctx = do
  output <- getOnlyOneOwnOutput ctx
  checkUpdateProtocolDatum inDatum newConf ctx output
  checkUpdateProtocolValue protocol ctx output
  pure ()

checkUpdateProtocolDatum :: Term s PProtocolDatum -> Term s PProtocolConfig -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolDatum inDatum newConf ctx txOut = do
  outDatum <- pfromData <$> getOwnOutputDatumFromTxOut ctx txOut
  pguardC "protocol constants shouldn't be changed" (pfield @"protocolConstants" # inDatum #== pfield @"protocolConstants" # outDatum)
  pguardC "protocol config changed unexpectable" (pfield @"protocolConfig" # outDatum #== newConf)
  pure ()

checkUpdateProtocolValue :: Term s PProtocol -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkUpdateProtocolValue protocol ctx txOut = do
  inValue <- getOwnInputValue ctx
  let outValue = getOwnOutputValueFromTxOut txOut
  pguardC "protocol value shouldn't be changed" (inValue #== outValue)
  let threadTokenAmount = pvalueOf # outValue # protocolSymbol protocol # protocolToken protocol
  pguardC "protocol thread token isn't in value" (threadTokenAmount #== 1)
  pure ()
