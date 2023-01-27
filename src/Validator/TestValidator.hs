module Validator.TestValidator where

import Plutarch.Api.V2
import qualified Plutarch.Api.V1.Value as PValue
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Extra.TermCont (pguardC)
import Plutarch.Prelude hiding (Generic)
import Plutarch.DataRepr
import Plutarch.Builtin
import qualified Plutarch.Monadic as P

data PTestRedeemer (s :: S)
  = PForbid (Term s (PDataRecord '[]))
  | PAllow (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PTestRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PTestRedeemer


testCheck :: ClosedTerm (PData :--> PData :--> PScriptContext :--> PUnit)
testCheck = plam $ \datm redm ctx -> P.do
      (red, _) <- ptryFrom @PTestRedeemer redm
      pmatch red $ \case
        PForbid _ -> perror
        PAllow _ -> pconstant ()

testValidator :: ClosedTerm PValidator
testValidator = plam $ \datm redm ctx -> popaque $ testCheck # datm # redm # ctx
