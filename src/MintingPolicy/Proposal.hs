module MintingPolicy.Proposal where

import qualified GHC.Generics as GHC
import Generics.SOP
import qualified Plutarch.Api.V1.Value as PValue
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.TermCont (pguardC, pletFieldsC, pletC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude hiding (Generic)
import Shared.Checks (checkMintingAmount, checkNftIsInTxInput)
import qualified MintingPolicy.NFT as NFT
import Governance.Proposal.Model (proposalThreadTokenName, proposalVerTokenName)
import Shared.ScriptContextV2 (getMintingTokenCurrency, getOnlyOneInputWithoutRef)

type PVerCs = PCurrencySymbol
type PVote = PInteger  -- 1 - for, 0 - against
type PAmount = PInteger
type PVoteTokenName = PTokenName

data PProposalPolicyRedeemer (s :: S)
  = PMintThreadToken (Term s (PDataRecord '["_0" ':= PTokenName]))
  | PBurnThreadToken (Term s (PDataRecord '["_0" ':= PTokenName]))
  | PMintVoteToken (Term s (PDataRecord '["_0" ':= PVerCs]))
  | PBurnVoteToken (Term s (PDataRecord '["_0" ':= PVoteTokenName, "_1" ':= PVerCs]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PProposalPolicyRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalPolicyRedeemer

proposalPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
proposalPolicy = plam $ \ref rdm' ctx -> P.do
  (rdm, _) <- ptryFrom @PProposalPolicyRedeemer rdm'
  pmatch rdm $ \case
    PMintThreadToken mintFields -> popaque $
      unTermCont $ do
        let tokenName = pfield @"_0" # mintFields
        checkMintingAmount 1 tokenName ctx
        NFT.checkUTxOSpent ref ctx
        pure $ pconstant ()
    PBurnThreadToken burnFields -> popaque $
      unTermCont $ do
        let tokenName = pfield @"_0" # burnFields
        checkMintingAmount (-1) tokenName ctx
        pure $ pconstant ()
    PMintVoteToken mintFields' -> popaque $
      unTermCont $ do
        let verCs = pfield @"_0" # mintFields'
        checkProposalTokensInInput verCs ctx
        pure $ pconstant ()
    PBurnVoteToken burnFields' -> popaque $
      unTermCont $ do
        burnFields <- pletFieldsC @["_0", "_1"] burnFields'
        checkProposalTokensInInput burnFields._1 ctx 
        checkMintingAmount (-1) burnFields._0 ctx
        pure $ pconstant ()

checkProposalTokensInInput :: Term s PCurrencySymbol -> Term s PScriptContext -> TermCont s ()
checkProposalTokensInInput verCs ctx = do
  ownCs <- pletC $ getMintingTokenCurrency # ctx
  proposalTn <- pletC $ proposalThreadTokenName
  let scriptInput = getOnlyOneInputWithoutRef # ctx
  checkNftIsInTxInput ownCs proposalTn ctx  -- Check voteTokenCs == proposalThreadTokenCs in input
  checkNftIsInTxInput verCs proposalVerTokenName ctx  -- Check proposal is verified

tokenNameSizeLimit :: Term s PInteger
tokenNameSizeLimit = pconstant 32

makeVoteTn :: Term s (PInteger :--> PInteger :--> PTokenName)
makeVoteTn  = phoistAcyclic $
  plam $ \vote amount -> P.do
    headV <- plet $ pconsBS # 68 # mempty -- 68 is ascii for "D"
    voteBs <- plet $ pencodeUtf8 # (pshow vote)
    amountBs <- plet $ pencodeUtf8 # (pshow amount)
    separator <- plet $ pconsBS # 46 # mempty -- 46 is ascii for "."
    tnBs <- plet $ headV <> voteBs <> separator <> amountBs
    plet tnBs $ \tnBs ->
      pif
        (plengthBS # tnBs #<= tokenNameSizeLimit)
        (pcon . PTokenName $ tnBs)
        (ptraceError "601")
