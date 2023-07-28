{-# LANGUAGE OverloadedRecordDot #-}

module Fundraising.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import Fundraising.Datum
import Fundraising.Model
import Fundraising.Redeemer
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Extra.Interval
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#*), (#+), (#-))
import Plutarch.Prelude hiding (pto)
import qualified Plutarch.Rational as Rational
import Protocol.Datum
import Protocol.Model
import Shared.Checks
import Shared.ScriptContextV2

fundraisingValidator :: ClosedTerm (PFundraising :--> PValidator)
fundraisingValidator = plam $ \fundraising datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PFundraisingDatum datm
  (red, _) <- ptryFrom @PFundraisingRedeemer redm
  frFields <- pletFields @["verTokenCurrency", "verTokenName", "protocol"] fundraising
  inputValue <- plet $ getOwnInputValue # ctx
  inputAda <- plet $ plovelaceValueOf # inputValue
  txInfo <- plet $ getCtxInfoForSpending # ctx
  datFields <- pletFields @["frDeadline", "frAmount", "creatorPkh", "managerAddress", "frFee"] dat
  protocolToken <- pletFields @["protocolCurrency", "protocolTokenName"] frFields.protocol
  pmatch red $ \case
    PDonate redData' -> popaque . unTermCont $ do
      output <- pletC $ getOnlyOneOwnOutput # ctx
      outputValue <- pletC $ pfield @"value" # output
      redData <- pletFieldsC @["_0", "_1", "_2"] redData'
      checkDonateDatum dat output
      checkDonateAdaValue datFields.frAmount inputAda outputValue redData._2
      checkNftIsInValue "405" frFields.verTokenCurrency frFields.verTokenName inputValue
      checkNftIsInValue "406" frFields.verTokenCurrency frFields.verTokenName outputValue
      checkNftIsInValue "407" redData._0 redData._1 inputValue
      checkNftIsInValue "408" redData._0 redData._1 outputValue
      checkDonatedBeforeDeadline datFields.frDeadline txInfo
      pure $ pconstant ()
    PReceiveFunds redData' -> popaque . unTermCont $ do
      redData <- pletFieldsC @["_0", "_1"] redData'
      raisedFunds <- pletC $ inputAda #- minTxOut #- minTxOut
      feePayment <- pletC $ calculateFees # datFields.frFee # raisedFunds
      validInterval <- pletC $ pfrom # datFields.frDeadline
      checkNftIsInValue "409" frFields.verTokenCurrency frFields.verTokenName inputValue
      checkNftIsInValue "410" redData._0 redData._1 inputValue
      checkNoOutputs ctx
      checkNftMinted "413" (-1) frFields.verTokenCurrency frFields.verTokenName txInfo
      checkNftMinted "414" (-1) redData._0 redData._1 txInfo
      checkIsSignedBy "411" datFields.creatorPkh txInfo
      checkFeePaid datFields.managerAddress feePayment ctx
      checkFundraisingCompleted datFields.frDeadline raisedFunds datFields.frAmount txInfo
      pure $ pconstant ()

checkDonateDatum :: Term s PFundraisingDatum -> Term s PTxOut -> TermCont s ()
checkDonateDatum inputDatum output = do
  outputDatum' <- pletC $ inlineDatumFromOutput # output
  (outputDatum, _) <- ptryFromC @PFundraisingDatum outputDatum'
  pguardC "401" (inputDatum #== outputDatum)

checkDonateAdaValue ::
  Term s PInteger ->
  Term s PInteger ->
  Term s SortedPositiveValue ->
  Term s PInteger ->
  TermCont s ()
checkDonateAdaValue maxAmount inputAda outputValue amt = do
  outputAda <- pletC $ plovelaceValueOf # outputValue
  pguardC "402" (outputAda #== (inputAda #+ amt))
  pguardC "403" (minTxOut #<= amt)
  pguardC "404" (inputAda #< maxAmount)

calculateFees :: Term s (PInteger :--> PInteger :--> PInteger)
calculateFees = phoistAcyclic $
  plam $ \fee' funds' ->
    let fee = (Rational.pfromInteger # (fee' #* funds')) Rational.#/ (Rational.pfromInteger # 100)
     in pmax # (pround # fee) # minTxOut

checkFundraisingCompleted ::
  Term s (PAsData PPOSIXTime) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PTxInfo) ->
  TermCont s ()
checkFundraisingCompleted deadline raisedFunds desiredFunds txInfo =
  pmatchC (desiredFunds #<= raisedFunds) >>= \case
    PTrue -> pure ()
    PFalse -> checkFundrisingCompletedTime deadline desiredFunds txInfo

checkFundrisingCompletedTime ::
  Term s (PAsData PPOSIXTime) ->
  Term s PInteger ->
  Term s (PAsData PTxInfo) ->
  TermCont s ()
checkFundrisingCompletedTime deadline desiredFunds txInfo = do
  txRange <- pletC $ pfield @"validRange" # txInfo
  calledReceiveFundsAt <- pletC $ pfromData $ getLowerBoundTime # txRange
  fundrisingInterval <- pletC $ pto # deadline
  pguardC "412" $ pafter # calledReceiveFundsAt # fundrisingInterval

checkDonatedBeforeDeadline :: Term s (PAsData PPOSIXTime) -> Term s (PAsData PTxInfo) -> TermCont s ()
checkDonatedBeforeDeadline deadline txInfo = do
  txRange <- pletC $ pfield @"validRange" # txInfo
  donatedAt <- pletC $ pfromData $ getLowerBoundTime # txRange
  fundrisingInterval <- pletC $ pto # deadline
  donatedAfterDeadline <- pletC $ pafter # donatedAt # fundrisingInterval
  pguardC "415" $ pnot # donatedAfterDeadline

checkFeePaid :: Term s (PAsData PAddress) -> Term s PInteger -> Term s PScriptContext -> TermCont s ()
checkFeePaid managerAddress fee ctx = do
  managerOutput <- pletC $ getOutputByAddress # ctx # (pfromData managerAddress)
  outputAda <- pletC $ plovelaceValueOf # (pfield @"value" # managerOutput)
  pguardC "203" (outputAda #== fee)
