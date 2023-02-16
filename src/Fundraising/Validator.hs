module Fundraising.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import Fundraising.Datum
import Fundraising.Model
import Fundraising.Redeemer
import Generics.SOP
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Bool (por')
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.Interval
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#*), (#+), (#-))
import Plutarch.Prelude hiding (pto)
import qualified Plutarch.Rational as Rational
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Model
import Shared.Checks
import Shared.ScriptContextV2

-- NOTE: `donate` and `close` endpoints (off-chain) must be provided with
-- mustValidateIn constraint to pass valid time range

fundraisingValidator :: ClosedTerm (PFundraising :--> PValidator)
fundraisingValidator = plam $ \fundraising datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PFundraisingDatum datm
  (red, _) <- ptryFrom @PFundraisingRedeemer redm
  let verTokenCS = pfield @"verTokenCurrency" # fundraising
      verTokenName = pfield @"verTokenName" # fundraising
      threadTokenCS = pfield @"threadTokenCurrency" # fundraising
      threadTokenName = pfield @"threadTokenName" # fundraising
      output = getOnlyOneOwnOutput # ctx
      inputValue = getOwnInputValue # ctx
      inputAda = plovelaceValueOf # inputValue
      outputValue = pfield @"value" # output
      txInfo = getCtxInfoForSpending # ctx
      deadline = pfield @"frDeadline" # dat
      desiredFunds = pfield @"frAmount" # dat
  pmatch red $ \case
    PDonate redData -> popaque $
      unTermCont $ do
        let amountToDonate = pfield @"_0" # redData
        checkDonateDatum dat ctx output
        checkDonateAdaValue desiredFunds inputAda outputValue amountToDonate
        checkNftIsInValue "405" verTokenCS verTokenName inputValue
        checkNftIsInValue "406" verTokenCS verTokenName outputValue
        checkNftIsInValue "407" threadTokenCS threadTokenName inputValue
        checkNftIsInValue "408" threadTokenCS threadTokenName outputValue
        checkDonatedBeforeDeadline deadline txInfo
        pure $ pconstant ()
    PReceiveFunds redData -> popaque $
      unTermCont $ do
        let creatorPkh = pfield @"creatorPkh" # dat
            fees = pfield @"frFee" # dat
            raisedFunds = inputAda #- minTxOut
            feePayment = calculateFees # fees # raisedFunds
            protocol = pfield @"protocol" # fundraising
            managerPkh = pfield @"managerPkh" # protocol
            validInterval = pfrom # deadline
        checkNftIsInValue "409" verTokenCS verTokenName inputValue
        checkNftIsInValue "410" threadTokenCS threadTokenName inputValue
        checkNoOutputs ctx
        checkNftMinted "413" (-1) verTokenCS verTokenName txInfo
        checkNftMinted "414" (-1) threadTokenCS threadTokenName txInfo
        checkIsSignedBy "411" creatorPkh txInfo
        checkPkhReceiveScriptValue managerPkh feePayment txInfo
        checkFundraisingCompleted validInterval raisedFunds desiredFunds txInfo
        pure $ pconstant ()

checkDonateDatum :: Term s PFundraisingDatum -> Term s PScriptContext -> Term s PTxOut -> TermCont s ()
checkDonateDatum inputDatum ctx output = do
  let outputDatum' = inlineDatumFromOutput # ctx # output
  (outputDatum, _) <- ptryFromC @PFundraisingDatum outputDatum'
  pguardC "401" (inputDatum #== outputDatum)
  pure ()

checkDonateAdaValue ::
  Term s PInteger ->
  Term s PInteger ->
  Term s SortedPositiveValue ->
  Term s PInteger ->
  TermCont s ()
checkDonateAdaValue maxAmount inputAda outputValue amt = do
  let outputAda = plovelaceValueOf # outputValue
  pguardC "402" (outputAda #== (inputAda #+ amt))
  pguardC "403" (minTxOut #<= amt)
  pguardC "404" (inputAda #< maxAmount)
  pure ()

calculateFees :: Term s (PInteger :--> PInteger :--> PInteger)
calculateFees = phoistAcyclic $
  plam $ \fee' funds' ->
    let fee = (Rational.pfromInteger # fee') Rational.#/ (Rational.pfromInteger # 100)
        funds = Rational.pfromInteger # funds'
        res = fee #* funds
     in pmax # (pround # res) # minTxOut

checkFundraisingCompleted ::
  Term s PPOSIXTimeRange ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PTxInfo) ->
  TermCont s ()
checkFundraisingCompleted afterDeadlineRange raisedFunds desiredFunds txInfo = do
  let txRange = pfield @"validRange" # txInfo
      timeClause = pcontains # afterDeadlineRange # txRange
      fundsClause = desiredFunds #<= raisedFunds
  pguardC "412" (por' # timeClause # fundsClause)

checkDonatedBeforeDeadline :: Term s (PAsData PPOSIXTime) -> Term s (PAsData PTxInfo) -> TermCont s ()
checkDonatedBeforeDeadline deadline txInfo = do
  let txRange = pfield @"validRange" # txInfo
  let donatedAt = pfromData $ getLowerBoundTime # txRange
  let fundrisingInterval = pto # deadline
  let donatedAfterDeadline = pafter # donatedAt # fundrisingInterval
  pguardC "415" $ pnot # donatedAfterDeadline
