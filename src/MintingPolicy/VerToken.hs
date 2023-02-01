module MintingPolicy.VerToken (verTokenPolicy) where 

import Plutarch.Api.V2
import qualified Plutarch.Api.V1.Value as PValue
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Extra.TermCont (pguardC)
import Plutarch.Prelude hiding (Generic)
import Plutarch.DataRepr
import Plutarch.Builtin
import qualified Plutarch.Monadic as P
import Protocol.Model
import Shared.Checks (checkMintingAmount, checkNftIsInTxInput, checkNftIsInTxOutput)

data PVerTokenRedeemer (s :: S) 
  = PMintVerToken (Term s (PDataRecord '["_0" ':= PTokenName])) 
  | PBurnVerToken (Term s (PDataRecord '["_0" ':= PTokenName])) 
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PVerTokenRedeemer where 
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVerTokenRedeemer

verTokenPolicy :: ClosedTerm (PProtocol :--> PMintingPolicy)
verTokenPolicy = plam $ \protocol rdm' ctx -> P.do
    (rdm, _) <- ptryFrom @PVerTokenRedeemer rdm'
    pmatch rdm $ \case 
      PMintVerToken mintFiends -> popaque $ unTermCont $ do
        let 
          verTokenName = pfield @"_0" # mintFiends
          protocolCurrency = pfield @"protocolCurrency" # protocol
          protocolTokenName = pfield @"protocolTokenName" # protocol
        checkMintingAmount 1 verTokenName ctx
        checkNftIsInTxInput protocolCurrency protocolTokenName ctx
        checkNftIsInTxOutput protocolCurrency protocolTokenName ctx
        pure $ pconstant ()
      PBurnVerToken burnFields -> popaque $ unTermCont $ do  
        let tn = pfield @"_0" # burnFields
        checkMintingAmount (-1) tn ctx
        pure $ pconstant ()
