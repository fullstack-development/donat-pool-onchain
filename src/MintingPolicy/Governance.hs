module MintingPolicy.Governance where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Plutarch.Api.V2 (PTokenName, PMintingPolicy, PPubKeyHash, PTxOutRef)
import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude hiding (Generic)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus
import Ply.Plutarch.Class (PlyArgOf)
import Plutarch.Extra.TermCont (pletC, pletFieldsC)
import MintingPolicy.NFT (checkUTxOSpent)
import Shared.Checks

govTokenName :: Term s PTokenName
govTokenName = pconstant . Plutus.TokenName $ Plutus.encodeUtf8 "DonatPool_governance_testnet"

data GovernanceTokensRedeemer (s :: S)
  = PMintGovernanceTokens (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PPubKeyHash]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType GovernanceTokensRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData GovernanceTokensRedeemer 

governancePolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
governancePolicy = plam $ \ref rdm' ctx -> P.do
  (rdm, _) <- ptryFrom @GovernanceTokensRedeemer rdm'
  txInfo <- plet $ pfield @"txInfo" # ctx
  pmatch rdm $ \case
    PMintGovernanceTokens mintFields -> popaque $
      unTermCont $ do
        amount <- pletC $ pfield @"_0" # mintFields
        red <- pletFieldsC @["_0", "_1"] mintFields
        checkUTxOSpent ref ctx
        checkMintingAmount red._0 govTokenName ctx
        checkIsSignedBy "549" red._1 txInfo 
        pure $ pconstant ()
