module Fundraising.Validator where

import Ext.Plutarch.Extra.ApiV2
import Ext.Plutus.MinAda
import Generics.SOP
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import PlutusCore (Closed)
import qualified PlutusCore as PLC
import Protocol.Model
import Shared.Checks
import Shared.ScriptContextV2
import Fundraising.Datum
import Fundraising.Model
import Fundraising.Redeemer

fundraisingValidator :: ClosedTerm (PFundraising :--> PValidator)
fundraisingValidator = plam $ \fundraising datm redm ctx -> P.do
  (dat, _) <- ptryFrom @PFundraisingDatum datm
  (red, _) <- ptryFrom @PFundraisingRedeemer redm
  pmatch red $ \case
    PDonate redData -> 
      -- check fundraising time range
      -- check amount to donate is greater than minAda
      -- check outputFunds == inputFunds + amountToDonate
      -- check outputDatum == inputDatum
      -- check fundraising script contains thread token and verification token
      popaque $ pconstant ()
    PReceiveFunds redData ->
      -- check fundraising time range is over OR the funds are fully raised
      -- check input contains threadToken and verToken
      -- check no own output
      -- check verToken and threadToken burned
      -- check the funds were transferred to user (??)
      -- check fees were transferred to protocol manager (?? or to protocol script - try transaction with 2 script redeemers)
      popaque $ pconstant ()
