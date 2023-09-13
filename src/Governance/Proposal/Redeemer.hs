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
type QuorumReached = PInteger   -- "no" = 0, "yes" = 1

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
    | PRejectProposal 
      (Term 
          s 
          ( PDataRecord
              '[ "_0" ':= ProposalThreadCs
              ,  "_1" ':= QuorumReached
              ]
          )
      )
    | PApplyProposal
            (Term 
          s 
          ( PDataRecord
              '[ "_0" ':= ProposalThreadCs
              ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PProposalRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalRedeemer