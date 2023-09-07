module MintingPolicy.VerToken (verTokenPolicy, feePoolVerTokenName) where

import FeePool.Models (feePoolThreadTokenName)
import qualified GHC.Generics as GHC
import Generics.SOP
import Governance.Validator (governanceThreadTokenName)
import Plutarch.Api.V2
import Plutarch.Extra.TermCont (pguardC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude hiding (Generic)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus
import Protocol.Model
import Shared.Checks (checkMintingAmount, checkNftIsInTxInput, checkNftIsInTxOutput)

feePoolVerTokenName :: Term s PTokenName
feePoolVerTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "FeePoolVerToken")

data PVerTokenRedeemer (s :: S)
  = PMintVerToken (Term s (PDataRecord '["_0" ':= PTokenName]))
  | PBurnVerToken (Term s (PDataRecord '["_0" ':= PTokenName]))
  | PMintProposalVerToken (Term s (PDataRecord '["_0" ':= PTokenName]))
  | PMintFeePoolVerToken (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PVerTokenRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVerTokenRedeemer

verTokenPolicy :: ClosedTerm (PProtocol :--> PMintingPolicy)
verTokenPolicy = plam $ \protocol rdm' ctx -> P.do
  (rdm, _) <- ptryFrom @PVerTokenRedeemer rdm'
  protocolCurrency <- plet $ pfield @"protocolCurrency" # protocol
  pmatch rdm $ \case
    PMintVerToken mintFields -> popaque $
      unTermCont $ do
        let verTokenName = pfield @"_0" # mintFields
            protocolTokenName = pfield @"protocolTokenName" # protocol
        checkMintingAmount 1 verTokenName ctx
        checkNftIsInTxInput protocolCurrency protocolTokenName ctx
        checkNftIsInTxOutput protocolCurrency protocolTokenName ctx
        pure $ pconstant ()
    PBurnVerToken burnFields -> popaque $
      unTermCont $ do
        let tn = pfield @"_0" # burnFields
        checkMintingAmount (-1) tn ctx
        pure $ pconstant ()
    PMintProposalVerToken mintFields -> popaque $
      unTermCont $ do
        let verTokenName = pfield @"_0" # mintFields
        checkMintingAmount 1 verTokenName ctx
        checkNftIsInTxInput protocolCurrency governanceThreadTokenName ctx
        checkNftIsInTxOutput protocolCurrency governanceThreadTokenName ctx
        pure $ pconstant ()
    PMintFeePoolVerToken _ -> popaque . unTermCont $ do
      checkMintingAmount 1 feePoolVerTokenName ctx
      checkNftIsInTxInput protocolCurrency feePoolThreadTokenName ctx
      pure $ pconstant ()
