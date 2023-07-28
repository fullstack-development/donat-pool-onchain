module Governance.Proposal.Model where

import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class
import PlutusLedgerApi.V1 (CurrencySymbol, TokenName)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusTx.Prelude as Plutus

data PProposal (s :: S)
  = PProposal
      ( Term
          s
          ( PDataRecord
              '[ 
                "protocolCurrency" ':= PCurrencySymbol,
                "govCurrency" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PProposal where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposal
instance PTryFrom PData (PAsData PProposal)

data Proposal = Proposal
  { protocolCurrency :: CurrencySymbol
  , verTokenCurrency :: TokenName
  }

type instance PlyArgOf PProposal = Proposal

proposalThreadTokenName :: Term s PTokenName
proposalThreadTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "ProposalId")

proposalVerTokenName :: Term s PTokenName
proposalVerTokenName = pconstant $ Plutus.TokenName (Plutus.encodeUtf8 "ProposalVerified")