{-# LANGUAGE OverloadedRecordDot #-}
module Governance.Proposal.Validator where


import Ext.Plutus.MinAda
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import Plutarch.Num (pnegate, (#+), (#-))
import qualified Plutarch.Monadic as P
import Shared.Checks (checkNftIsInValue, checkIsSignedBy, checkNftMinted, checkNoOutputs)
import Shared.ScriptContextV2
import Governance.Datum (PGovernanceDatum(PGovernanceDatum))
import Governance.Proposal.Redeemer ( PProposalRedeemer (PVote))
import Data.Data
import PlutusLedgerApi.V1 (PubKeyHash)
import Governance.Proposal.Model (PProposal, proposalThreadTokenName, proposalVerTokenName)
import Plutarch.Prelude hiding (pto)
import Plutarch.Extra.Interval
import Governance.Proposal.Datum (PProposalDatum(PProposalDatum))
import Ext.Plutarch.Extra.Bool (integerToBool)
import Governance.Validator (getGovernanceDatumFromReferenceUtxo)
import MintingPolicy.Proposal (makeVoteTn)

proposalValidator :: ClosedTerm (PProposal :--> PValidator)
proposalValidator = phoistAcyclic $
  plam $ \proposal' dat' rdm' ctx -> P.do
    (dat, _) <- ptryFrom @PProposalDatum dat'
    (red, _) <- ptryFrom @PProposalRedeemer rdm'
    txInfo <- plet $ pfield @"txInfo" # ctx
    input <- plet $ getOwnInputOrTraceError # ctx
    inValue <- plet $ pfield @"value" # input
    proposal <- pletFields @["protocolCurrency", "adaoCurrency", "verTokenCurrency"] proposal'
    
    pmatch red $ \case
        PVote redData' -> popaque . unTermCont $ do
            redData <- pletFieldsC @["_0", "_1", "_2", "_3"] redData' -- isVoteFor (0/1), Amount, VoterAddress, ProposalThreadTn
            inDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "deadline", "applied"] dat
            
            proposalOutput <- pletC $ getOnlyOneOwnOutput # ctx
            outputDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
            (outputDatum, _) <- tcont $ ptryFrom @PProposalDatum outputDatum'
            outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "deadline", "applied"] outputDatum
            outValue <- pletC $ pfield @"value" # proposalOutput

            checkNftIsInValue "901" redData._3 proposalThreadTokenName inValue
            checkNftIsInValue "902" proposal.verTokenCurrency proposalVerTokenName inValue
            voterPkh <- pletC $ extractPaymentPkhFromAddress # redData._2
            checkIsSignedBy "903" voterPkh txInfo
            pguardC "912" $ inDatum.applied #== pdata 0
            checkVotedBeforeDeadline inDatum.deadline txInfo

            pguardC "904" $ inDatum.proposal #== outDatum.proposal
            pguardC "905" $ inDatum.policyRef #== outDatum.policyRef
            pguardC "906" $ inDatum.quorum #== outDatum.quorum
            pguardC "907" $ inDatum.initiator #== outDatum.initiator 
            pguardC "908" $ inDatum.deadline #== outDatum.deadline --
            pguardC "909" $ inDatum.applied #== outDatum.applied --

            for <- pletC $ integerToBool # redData._0
            checkVoteValuesInDatum for redData._1 inDatum.for outDatum.for inDatum.against outDatum.against
            checkOutputValue inValue outValue proposal.protocolCurrency redData._1 ctx
            checkPaymentToVoter voterPkh redData._0 redData._1 redData._3 txInfo
            pure $ pconstant ()

checkVoteValuesInDatum :: 
  Term s PBool 
  -> Term s PInteger 
  -> Term s PInteger 
  -> Term s PInteger 
  -> Term s PInteger 
  -> Term s PInteger 
  -> TermCont s ()
checkVoteValuesInDatum isVotedFor amount inFor outFor inAgainst outAgainst = do
    pmatchC isVotedFor >>= \case
      PTrue ->
        pguardC "908" $ outFor #== inFor #+ amount #&& outAgainst #== inAgainst
      PFalse ->
        pguardC "909" $  outAgainst #== inAgainst #+ amount #&& outFor #== inFor

checkOutputValue :: 
  Term s (PValue 'Sorted 'Positive)
  -> Term s (PValue 'Sorted 'Positive)
  -> Term s PCurrencySymbol
  -> Term s PInteger
  -> Term s PScriptContext
  -> TermCont s ()
checkOutputValue inValue' outValue' protocolCs amount ctx = do
  inValue <- pletC $ Value.pforgetPositive inValue'
  outValue <- pletC $ Value.pforgetPositive outValue'
  govDatum' <- pletC $ getGovernanceDatumFromReferenceUtxo # protocolCs # ctx
  govDatum <- pletFieldsC @["govCurrency", "govTokenName"] govDatum'
  expectedAdaoAdding <- pletC $ Value.psingleton # govDatum.govCurrency # govDatum.govTokenName # amount
  expectedOutputValue <- pletC $ inValue <> expectedAdaoAdding <> minAdaValue
  pguardC "910" (expectedOutputValue #== outValue)

checkPaymentToVoter :: 
  Term s PPubKeyHash
  -> Term s PInteger
  -> Term s PInteger
  -> Term s PCurrencySymbol
  -> Term s (PAsData PTxInfo)
  -> TermCont s ()
checkPaymentToVoter pkh vote amount proposalCs txInfo = do
  voteTokenName <- pletC $ makeVoteTn # vote # amount
  paymentToVoter <- pletC $ 
              Value.psingleton # proposalCs # voteTokenName # 1
                <> minAdaValue
  pguardC "911" (pubKeyOutputContainsValue # pkh # txInfo # paymentToVoter)

checkVotedBeforeDeadline :: Term s (PAsData PPOSIXTime) -> Term s (PAsData PTxInfo) -> TermCont s ()
checkVotedBeforeDeadline deadline txInfo = do
  txRange <- pletC $ pfield @"validRange" # txInfo
  votedAt <- pletC $ pfromData $ getLowerBoundTime # txRange
  proposalInterval <- pletC $ pto # deadline
  votedAfterDeadline <- pletC $ pafter # votedAt # proposalInterval
  pguardC "913" $ pnot # votedAfterDeadline
