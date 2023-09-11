{-# LANGUAGE OverloadedRecordDot #-}
module Governance.Proposal.Validator where


import Ext.Plutus.MinAda
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import Plutarch.Num (pnegate, (#+), (#-))
import qualified Plutarch.Monadic as P
import Shared.Checks (checkNftIsInValue, checkIsSignedBy, checkNftMinted, checkNoOutputs, checkPaidToWalletAddress)
import Shared.ScriptContextV2
import Governance.Datum (PGovernanceDatum(PGovernanceDatum))
import Governance.Proposal.Redeemer ( PProposalRedeemer (..))
import Data.Data
import PlutusLedgerApi.V1 (PubKeyHash)
import Governance.Proposal.Model (PProposal, proposalThreadTokenName, proposalVerTokenName)
import Plutarch.Prelude hiding (pto)
import Plutarch.Extra.Interval
import Governance.Proposal.Datum (PProposalDatum(PProposalDatum))
import Ext.Plutarch.Extra.Bool (integerToBool)
import Governance.Validator (getGovernanceDatumFromReferenceUtxo)
import MintingPolicy.Proposal (makeVoteTn)
import Protocol.Model (PProtocol (..), Protocol (protocolCurrency))
import Protocol.Validator (getProtocolDatumFromReferenceUtxoByToken)
import Plutarch.Api.V1.Value (plovelaceValueOf)

proposalValidator :: ClosedTerm (PProposal :--> PValidator)
proposalValidator = phoistAcyclic $
  plam $ \proposal' dat' rdm' ctx -> P.do
    (dat, _) <- ptryFrom @PProposalDatum dat'
    (red, _) <- ptryFrom @PProposalRedeemer rdm'
    txInfo <- plet $ pfield @"txInfo" # ctx
    input <- plet $ getOwnInputOrTraceError # ctx
    inValue <- plet $ pfield @"value" # input
    proposal <- pletFields @["protocolCurrency", "protocolTokenName", "verTokenCurrency"] proposal'
    
    pmatch red $ \case
        PVote redData' -> popaque . unTermCont $ do
            -- redData: isVoteFor (0/1), Amount, VoterAddress, ProposalThreadTn
            redData <- pletFieldsC @["_0", "_1", "_2", "_3"] redData' 
            inDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "cost", "deadline", "processed"] dat
            
            proposalOutput <- pletC $ getOnlyOneOwnOutput # ctx
            outputDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
            (outputDatum, _) <- tcont $ ptryFrom @PProposalDatum outputDatum'
            outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "cost", "deadline", "processed"] outputDatum
            outValue <- pletC $ pfield @"value" # proposalOutput

            checkNftIsInValue "901" redData._3 proposalThreadTokenName inValue
            checkNftIsInValue "902" proposal.verTokenCurrency proposalVerTokenName inValue

            voterPkh <- pletC $ extractPaymentPkhFromAddress # redData._2
            checkIsSignedBy "903" voterPkh txInfo

            pguardC "912" $ inDatum.processed #== pdata 0
            checkVotedBeforeDeadline inDatum.deadline txInfo
            pguardC "904" $ inDatum.proposal #== outDatum.proposal
            pguardC "905" $ inDatum.policyRef #== outDatum.policyRef
            pguardC "906" $ inDatum.quorum #== outDatum.quorum
            pguardC "907" $ inDatum.initiator #== outDatum.initiator 
            pguardC "914" $ inDatum.deadline #== outDatum.deadline
            pguardC "915" $ inDatum.processed #== outDatum.processed
            pguardC "921" $ inDatum.cost #== outDatum.cost

            for <- pletC $ integerToBool # redData._0
            checkVoteValuesInDatum for redData._1 inDatum.for outDatum.for inDatum.against outDatum.against
            checkOutputValue inValue outValue proposal.protocolCurrency redData._1 ctx
            checkPaymentToVoter voterPkh redData._0 redData._1 redData._3 txInfo
            pure $ pconstant ()

        PRejectProposal redData' -> popaque . unTermCont $ do
            -- redData: ProposalThreadCs, QuorumReached
            redData <- pletFieldsC @["_0", "_1"] redData'
            inDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "cost", "deadline", "processed"] dat
            
            proposalOutput <- pletC $ getOnlyOneOwnOutput # ctx
            outputDatum' <- pletC $ inlineDatumFromOutput # proposalOutput
            (outputDatum, _) <- tcont $ ptryFrom @PProposalDatum outputDatum'
            outDatum <- pletFieldsC @["proposal", "for", "against", "policyRef", "quorum", "initiator", "cost", "deadline", "processed"] outputDatum
            outValue <- pletC $ pfield @"value" # proposalOutput

            checkNftIsInValue "901" redData._0 proposalThreadTokenName inValue
            checkNftIsInValue "902" proposal.verTokenCurrency proposalVerTokenName inValue
            
            checkStartProcessAfterDeadline inDatum.deadline txInfo
            pguardC "919" $ pnot #$ shouldApplyVotingResults # outDatum.quorum # outDatum.for # outDatum.against

            pguardC "904" $ inDatum.proposal #== outDatum.proposal
            pguardC "906" $ inDatum.for #== outDatum.for
            pguardC "906" $ inDatum.against #== outDatum.against
            pguardC "905" $ inDatum.policyRef #== outDatum.policyRef
            pguardC "906" $ inDatum.quorum #== outDatum.quorum
            pguardC "907" $ inDatum.initiator #== outDatum.initiator 
            pguardC "914" $ inDatum.deadline #== outDatum.deadline
            pguardC "912" $ inDatum.processed #== pdata 1
            pguardC "921" $ inDatum.cost #== outDatum.cost 

            protocolDatum' <- pletC $ getProtocolDatumFromReferenceUtxoByToken # proposal.protocolCurrency # proposal.protocolTokenName # ctx
            managerAddress <- pletC $ pfield @"managerAddress" # protocolDatum'

            checkValue inValue outValue managerAddress redData._1 inDatum.cost ctx
            checkManagerSignedTx managerAddress ctx txInfo
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
  expectedGovTokensAmount <- pletC $ Value.psingleton # govDatum.govCurrency # govDatum.govTokenName # amount
  expectedOutputValue <- pletC $ inValue <> expectedGovTokensAmount <> minAdaValue
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
  checkNftMinted "413" 1 proposalCs voteTokenName txInfo
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

checkStartProcessAfterDeadline :: Term s (PAsData PPOSIXTime) -> Term s (PAsData PTxInfo) -> TermCont s ()
checkStartProcessAfterDeadline deadline txInfo = do
  txRange <- pletC $ pfield @"validRange" # txInfo
  startProcessAt <- pletC $ pfromData $ getLowerBoundTime # txRange
  proposalInterval <- pletC $ pto # deadline
  startedProcessAfterDeadline <- pletC $ pafter # startProcessAt # proposalInterval
  pguardC "913" startedProcessAfterDeadline

shouldApplyVotingResults :: Term s (PInteger :--> PInteger :--> PInteger :--> PBool) 
shouldApplyVotingResults = plam $ \quorum for against ->
    quorum #<= (for #+ against) #&& against #< for

checkManagerSignedTx :: 
  Term s PAddress
  -> Term s PScriptContext
  -> Term s (PAsData PTxInfo) 
  -> TermCont s ()
checkManagerSignedTx managerAddress ctx txInfo = do
  managerPkh <- pletC $ extractPaymentPkhFromAddress # managerAddress
  checkIsSignedBy "920" managerPkh txInfo

checkValue :: 
  Term s SortedPositiveValue
  -> Term s SortedPositiveValue
  -> Term s PAddress
  -> Term s PInteger
  -> Term s PInteger
  -> Term s PScriptContext
  -> TermCont s ()
checkValue inValue outValue managerAddress quorumReached proposalCost ctx = do
  quorumIsReached <- pletC $ integerToBool # quorumReached
  pmatchC quorumIsReached >>= \case
    PTrue -> pguardC "917" $ inValue #== outValue
    PFalse -> do
      inputAda <- pletC $ plovelaceValueOf # inValue
      outputAda <- pletC $ plovelaceValueOf # outValue
      pguardC "922" $ outputAda #== inputAda - proposalCost
      checkPaidToWalletAddress managerAddress proposalCost ctx
