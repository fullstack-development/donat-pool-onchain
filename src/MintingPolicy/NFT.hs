module MintingPolicy.NFT (nftPolicy) where 

import Plutarch.Api.V1
import qualified Plutarch.Api.V1.Value as PValue
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Extra.TermCont (pguardC)
import Plutarch.Prelude hiding (Generic)
import Plutarch.DataRepr
import Plutarch.Builtin
import qualified Plutarch.Monadic as P

data PNftRedeemer (s :: S) 
  = PMintNft (Term s (PDataRecord '["_0" ':= PTokenName])) 
  | PBurnNft (Term s (PDataRecord '["_0" ':= PTokenName])) 
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PNftRedeemer where 
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PNftRedeemer

nftPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
nftPolicy = plam $ \ref rdm' ctx -> P.do
    (rdm, _) <- ptryFrom @PNftRedeemer rdm'
    pmatch rdm $ \case 
      PMintNft mintFiends -> popaque $ unTermCont $ do
        let tn = pfield @"_0" # mintFiends
        checkMintingAmount 1 tn ctx
        checkUTxOSpent ref ctx
        pure $ pconstant ()
      PBurnNft burnFields -> popaque $ unTermCont $ do 
        let tn = pfield @"_0" # burnFields
        checkMintingAmount (-1) tn ctx
        pure $ pconstant ()

checkMintingAmount :: Term s PInteger -> Term s PTokenName -> Term s PScriptContext -> TermCont s ()
checkMintingAmount amt tn ctx' = do 
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["mint"] $ getField @"txInfo" ctx
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== amt

checkUTxOSpent :: Term s PTxOutRef -> Term s PScriptContext -> TermCont s ()
checkUTxOSpent ref ctx' = do 
    ctx <- tcont $ pletFields @'["txInfo"] ctx'
    txInfo <- tcont $ pletFields @'["inputs"] $ getField @"txInfo" ctx
    pguardC "UTxO not consumed" $
      (pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $ getField @"inputs" txInfo)
