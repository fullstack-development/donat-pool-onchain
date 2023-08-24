module Governance.Redeemer where

import qualified GHC.Generics as GHC
import Governance.Proposal.Datum
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude

type PProposalAddress = PAddress
type PProposalThreadCurrency = PCurrencySymbol
type PProposalVerCurrency = PCurrencySymbol
type PProposalStartedAt = PPOSIXTime

data PGovernanceRedeemer (s :: S)
  = PCreateProposal
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PProposalParameters
               , "_1" ':= PProposalAddress
               , "_2" ':= PProposalThreadCurrency
               , "_3" ':= PProposalVerCurrency
               , "_4" ':= PProposalStartedAt
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PGovernanceRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PGovernanceRedeemer
