{-# LANGUAGE OverloadedRecordDot #-}

module Fundraising.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import FeePool.Models
import Fundraising.Datum
import Fundraising.Model
import Fundraising.Redeemer
import MintingPolicy.VerToken (feePoolVerTokenName)
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
  twoMinAdaAmt <- plet $ minTxOut #+ minTxOut
  raisedFunds <- plet $ inputAda #- twoMinAdaAmt
  feeAmount <- plet $ calculateFees # datFields.frFee # raisedFunds
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
    
    PReceiveFundsCurrentEpoch redData' -> popaque . unTermCont $ do
      redData <- pletFieldsC @["_0", "_1"] redData'
      pguardC "416" (twoMinAdaAmt #< feeAmount)
      checkFeePaidCurrentEpoch protocolToken.protocolCurrency datFields.managerAddress feeAmount ctx
      checkFundraisingCompleted datFields.frDeadline raisedFunds datFields.frAmount txInfo
      receiveFundsCommonChecks frFields.verTokenCurrency frFields.verTokenName redData._0 redData._1 inputValue datFields.creatorPkh ctx
      pure $ pconstant ()
    
    PReceiveFundsNewEpoch redData' -> popaque . unTermCont $ do
      redData <- pletFieldsC @["_0", "_1"] redData'
      pguardC "416" (twoMinAdaAmt #< feeAmount)
      checkFeePaidNewEpoch protocolToken.protocolCurrency frFields.verTokenCurrency datFields.managerAddress feeAmount ctx
      checkFundraisingCompleted datFields.frDeadline raisedFunds datFields.frAmount txInfo
      receiveFundsCommonChecks frFields.verTokenCurrency frFields.verTokenName redData._0 redData._1 inputValue datFields.creatorPkh ctx
      pure $ pconstant ()
    
    PReceiveFundsWithoutFee redData' -> popaque . unTermCont $ do
      redData <- pletFieldsC @["_0", "_1"] redData'
      pguardC "419" (feeAmount #<= twoMinAdaAmt)
      feePaidToManager datFields.managerAddress feeAmount ctx
      checkFundraisingCompleted datFields.frDeadline raisedFunds datFields.frAmount txInfo
      receiveFundsCommonChecks frFields.verTokenCurrency frFields.verTokenName redData._0 redData._1 inputValue datFields.creatorPkh ctx
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

feePaidToManager :: Term s (PAsData PAddress) -> Term s PInteger -> Term s PScriptContext -> TermCont s ()
feePaidToManager managerAddress feeAmt ctx = do
  managerOutput <- pletC $ getOutputByAddress # ctx # (pfromData managerAddress)
  outputAda <- pletC $ plovelaceValueOf # (pfield @"value" # managerOutput)
  pguardC "203" (outputAda #== feeAmt)

feePaidToFeePool :: Term s PCurrencySymbol -> Term s PInteger -> Term s PScriptContext -> TermCont s ()
feePaidToFeePool systemCurrency expectedFee ctx = do
  feePoolInput <- pletC $ getOnlyOneInputByToken # systemCurrency # feePoolThreadTokenName # ctx
  feePoolOutput <- pletC $ getOnlyOneOutputByToken # systemCurrency # feePoolThreadTokenName # ctx
  feePoolInputAda <- pletC $ plovelaceValueOf # (pfield @"value" # feePoolInput)
  feePoolOutputAda <- pletC $ plovelaceValueOf # (pfield @"value" # feePoolOutput)
  expectedOutputAda <- pletC $ feePoolInputAda #+ expectedFee
  pguardC "417" (feePoolOutputAda #== expectedOutputAda)

checkFeePaidCurrentEpoch :: Term s PCurrencySymbol -> Term s (PAsData PAddress) -> Term s PInteger -> Term s PScriptContext -> TermCont s ()
checkFeePaidCurrentEpoch systemCurrency managerAddress feeAmount ctx = do
  feePaidToManager managerAddress minTxOut ctx
  feePaidToFeePool systemCurrency (feeAmount #- minTxOut) ctx

checkFeePaidNewEpoch :: Term s PCurrencySymbol -> Term s PCurrencySymbol -> Term s (PAsData PAddress) -> Term s PInteger -> Term s PScriptContext -> TermCont s ()
checkFeePaidNewEpoch systemCurrency verTokenCurrency managerAddress feeAmount ctx = do
  feePaidToManager managerAddress minTxOut ctx
  feePaidToFeePool systemCurrency (feeAmount #- minTxOut #- minTxOut) ctx

  feePoolInfoOutput <- pletC $ getOnlyOneOutputByToken # verTokenCurrency # feePoolVerTokenName # ctx
  feePoolInfoOutputAda <- pletC $ plovelaceValueOf # (pfield @"value" # feePoolInfoOutput)
  pguardC "418" (feePoolInfoOutputAda #== minTxOut)

receiveFundsCommonChecks :: 
  Term s PCurrencySymbol -> 
  Term s PTokenName -> 
  Term s PCurrencySymbol -> 
  Term s PTokenName ->
  Term s SortedPositiveValue ->
  Term s PPubKeyHash ->
  Term s PScriptContext -> 
  TermCont s ()
receiveFundsCommonChecks verTokenCurrency verTokenName threadTokenCurrency threadTokenName inputValue creatorPkh ctx = do
  txInfo <- pletC $ getCtxInfoForSpending # ctx
  checkNftIsInValue "409" verTokenCurrency verTokenName inputValue
  checkNftIsInValue "410" threadTokenCurrency threadTokenName inputValue
  checkNoOutputs ctx
  checkNftMinted "413" (-1) verTokenCurrency verTokenName txInfo
  checkNftMinted "414" (-1) threadTokenCurrency threadTokenName txInfo
  checkIsSignedBy "411" creatorPkh txInfo
