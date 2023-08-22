module Governance.Proposal.Redeemer where

import qualified GHC.Generics as GHC
import Governance.Proposal.Datum
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude

type PIsVoteFor = PInteger   -- "against" = 0, "for" = 1
type PAmount = PInteger
type PVoter = PAddress
type ProposalThreadCs = PCurrencySymbol

data PProposalRedeemer (s :: S)
  = PVote
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PIsVoteFor -- "against" = 0, "for" = 1,
               , "_1" ':= PAmount
               , "_2" ':= PVoter
               , "_3" ':= ProposalThreadCs
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PProposalRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalRedeemer