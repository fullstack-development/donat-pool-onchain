module Validator.AlwaysSucceeds where

import Plutarch.Api.V1 (PValidator)
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts
import Plutarch.Prelude

alwaysSucceeds :: ClosedTerm (PData :--> PData :--> PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()

alwaysSucceedsValidator :: ClosedTerm PValidator
alwaysSucceedsValidator = plam $ \datm redm ctx -> popaque $ alwaysSucceeds # datm # redm # ctx
