module Governance.Proposal.Redeemer where

import qualified GHC.Generics as GHC
import Governance.Proposal.Datum
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude

type IsVoteFor = PInteger   -- "against" = 0, "for" = 1
type Voter = PPubKeyHash

data PProposalRedeemer (s :: S)
  = PVote
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= IsVoteFor -- "against" = 0, "for" = 1,
               , "_1" ':= Voter
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PProposalRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalRedeemer
