module MintingPolicy.NFT (nftPolicy) where 

import Plutarch.Api.V1
import qualified Plutarch.Api.V1.Value as PValue
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude hiding (Generic)
import Plutarch.DataRepr
import Plutarch.Builtin
import qualified Plutarch.Monadic as P

data PNftRedeemer (s :: S) 
  = PMintNft (Term s (PDataRecord '[])) 
  | PBurnNft (Term s (PDataRecord '[])) 
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PNftRedeemer where 
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PNftRedeemer

nftPolicy :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftPolicy = plam $ \ref tn rdm' ctx' -> P.do
    (rdm, _) <- ptryFrom @PNftRedeemer rdm'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
    pmatch rdm $ \case 
      PMintNft _ -> popaque $
        pif 
          (pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $ getField @"inputs" txInfo)
          (pif 
              (PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== 1)
              (pconstant ())
              (ptraceError "Wrong NFT mint amount"))
          (ptraceError "UTxO not consumed")
      PBurnNft _ -> popaque $ 
        pif 
            (PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== -1)
            (pconstant ())
            (ptraceError "Wrong NFT mint amount")
