module Internal.Constraint (module TcPluginM, flatToCt, overEvidencePredType) where

import GhcApi.GhcPlugins
import GhcApi.Constraint
  (Ct(..), CtEvidence(..), ctLoc, ctEvId, mkNonCanonical)

import TcType (TcType)
import TcPluginM (newGiven)

flatToCt :: [((TcTyVar,TcType),Ct)] -> Maybe Ct
flatToCt [((_,lhs),ct),((_,rhs),_)]
  = Just
  $ mkNonCanonical
  $ CtGiven (mkPrimEqPred lhs rhs)
            (ctEvId ct)
            (ctLoc ct)

flatToCt _ = Nothing

-- | Modify the predicate type of the evidence term of a constraint
overEvidencePredType :: (TcType -> TcType) -> Ct -> Ct
overEvidencePredType f ct =
  let
    ev :: CtEvidence
    ev = cc_ev ct
  in ct { cc_ev = ev { ctev_pred = f (ctev_pred ev) } }