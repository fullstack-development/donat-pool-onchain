module Fundraising.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import Generics.SOP
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Interval
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#+))
import Plutarch.Prelude hiding (pto)
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Model
import Shared.Checks
import Shared.ScriptContextV2
import Fundraising.Datum
import Fundraising.Model
import Fundraising.Redeemer

-- NOTE: `donate` and `close` endpoints (off-chain) must be provided with 
-- mustValidateIn constraint to pass valid time range

fundraisingValidator :: ClosedTerm (PFundraising :--> PValidator)
fundraisingValidator = plam $ \fundraising datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PFundraisingDatum datm
  (red, _) <- ptryFrom @PFundraisingRedeemer redm
  let 
    verTokenCS = pfield @"verTokenCurrency" # fundraising
    verTokenName = pfield @"verTokenName" # fundraising
    output = getOnlyOneOwnOutput # ctx
    inputValue = getOwnInputValue # ctx
    outputValue = pfield @"value" # output
    txInfo = pfield @"txInfo" # ctx
    deadline = pfield @"frDeadline" # dat
  pmatch red $ \case
    PDonate redData -> popaque $ unTermCont $ do
      let 
        amountToDonate = pfield @"_2" # redData
        threadTokenCS = pfield @"_0" # redData
        threadTokenName = pfield @"_1" # redData
        validInterval = pto # deadline
      checkDonateDatum dat ctx output
      checkDonateAdaValue dat inputValue outputValue amountToDonate
      checkNftIsInValue "405" verTokenCS verTokenName inputValue
      checkNftIsInValue "406" verTokenCS verTokenName outputValue
      checkNftIsInValue "407" threadTokenCS threadTokenName inputValue
      checkNftIsInValue "408" threadTokenCS threadTokenName outputValue
      checkValidTimeRange validInterval txInfo
      pure $ pconstant ()
    PReceiveFunds redData ->
      -- check fundraising time range is over OR the funds are fully raised
      -- check input contains threadToken and verToken
      -- check no own output
      -- check verToken and threadToken burned
      -- check the funds were transferred to user (??)
      -- check fees were transferred to protocol manager (?? or to protocol script - try transaction with 2 script redeemers)
      popaque $ pconstant ()

checkDonateDatum :: Term s PFundraisingDatum -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkDonateDatum inputDatum ctx output = do
  let 
    outputDatum' = inlineDatumFromOutput # ctx # output
  (outputDatum, _) <- ptryFromC @PFundraisingDatum outputDatum'
  pguardC "401" (inputDatum #== outputDatum)
  pure ()

checkDonateAdaValue :: 
  Term s PFundraisingDatum -> 
  Term s SortedPositiveValue -> 
  Term s SortedPositiveValue -> 
  Term s PInteger -> 
  TermCont s ()
checkDonateAdaValue inputDatum inputValue outputValue amt = do 
  let 
    inputAda = plovelaceValueOf # inputValue
    outputAda = plovelaceValueOf # outputValue
    maxAmount = pfield @"frAmount" # inputDatum 
  pguardC "402" (outputAda #== (inputAda #+ amt))
  pguardC "403" (minTxOut #<= amt)
  pguardC "404" (inputAda #< maxAmount)
  pure ()
