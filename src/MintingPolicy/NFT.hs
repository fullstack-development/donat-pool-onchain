module MintingPolicy.NFT (nftPolicy) where 

import Plutarch.Api.V1
import qualified Plutarch.Api.V1.Value as PValue
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude hiding (Generic)
import Plutarch.DataRepr
import Plutarch.Builtin

data PNftRedeemer (s :: S) 
  = --PMintingAction (Term s (PDataRecord '["isMinting" :: PBool])) 
  PMintNft (Term s (PDataRecord '[])) 
  | PBurnNft (Term s (PDataRecord '[])) 
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PNftRedeemer where 
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PNftRedeemer



nftPolicy :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftPolicy = plam $ \ref tn rdm' ctx' -> popaque $
  unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
    
    -- add getting redeemer 
    rdm <- fst <$> tcont $ ptryFrom @PNftRedeemer rdm'
   -- rdm <- rdmM
   -- rdm <- tcont . pmatch . fst <$> ptryFrom @PNftRedeemer rdm'
    case rdm of 
      PMintNft _ -> do     
        pguardC "UTxO not consumed" $
          pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $
            getField @"inputs" txInfo
        pguardC "Wrong NFT mint amount" $
          PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== 1
    --  _ -> tcont $ ptraceError "Wrong Redeemer type"
    pure $ pconstant ()

pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s
