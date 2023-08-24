module MintingPolicy.NFT (nftPolicy, checkUTxOSpent) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Api.V2
import Plutarch.Extra.TermCont (pguardC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude hiding (Generic)
import Shared.Checks (checkMintingAmount)

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
    PMintNft mintFiends -> popaque $
      unTermCont $ do
        let tn = pfield @"_0" # mintFiends
        checkMintingAmount 1 tn ctx
        checkUTxOSpent ref ctx
        pure $ pconstant ()
    PBurnNft burnFields -> popaque $
      unTermCont $ do
        let tn = pfield @"_0" # burnFields
        checkMintingAmount (-1) tn ctx
        pure $ pconstant ()

checkUTxOSpent :: Term s PTxOutRef -> Term s PScriptContext -> TermCont s ()
checkUTxOSpent ref ctx' = do
  ctx <- tcont $ pletFields @'["txInfo"] ctx'
  txInfo <- tcont $ pletFields @'["inputs"] $ getField @"txInfo" ctx
  pguardC "UTxO not consumed" $
    (pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $ getField @"inputs" txInfo)
