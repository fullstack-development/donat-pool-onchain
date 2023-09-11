{-# LANGUAGE OverloadedRecordDot #-}

module Governance.Proposal.Datum where

import Data.Typeable
import qualified GHC.Generics as GHC
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Prelude
import Ply.Plutarch.Class
import qualified Plutarch.Monadic as P
import Protocol.Model

data PProposalDatum (s :: S)
    = PProposalDatum
        (Term
          s
          ( PDataRecord
              '[ "proposal" ':= PProposalParameters
               , "for" ':= PInteger
               , "against" ':= PInteger
               , "policyRef" ':= PTxOutRef
               , "quorum" ':= PInteger
               , "initiator" ':= PAddress
               , "cost" ':= PInteger 
               , "deadline" ':= PPOSIXTime
               , "processed" ':= PInteger  -- Bool
               ]
          )
        )
    deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PProposalDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalDatum 
instance PTryFrom PData (PAsData PProposalDatum)

data PProposalParameters (s :: S) =
  PProposalParameters 
    ( Term
          s
          ( PDataRecord
              '[ "minAmount" ':= PInteger
               , "maxAmount" ':= PInteger
               , "minDuration" ':= PInteger
               , "maxDuration" ':= PInteger
               , "protocolFee" ':= PInteger
               ]
          )
      )

  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PProposalParameters where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PProposalParameters
