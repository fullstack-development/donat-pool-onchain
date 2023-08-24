module Ext.Plutarch.Extra.Bool where

import Plutarch.Prelude hiding (Generic)

integerToBool :: Term s (PInteger :--> PBool)
integerToBool = phoistAcyclic $ 
  plam $ \flag -> 
    pif (flag #== (1 :: Term s PInteger)) (pcon PTrue) $
      pif (flag #== (0 :: Term s PInteger)) (pcon PFalse) $ ptraceError "501"
