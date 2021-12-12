module Internal.Constraint (newGiven, flatToCt, overEvidencePredType) where

import GhcApi.GhcPlugins
import GhcApi.Constraint
  (Ct(..), CtEvidence(..), CtLoc, ctLoc, ctEvId, mkNonCanonical)

import Panic (panicDoc)
import TcType (TcType)
import Constraint (QCInst(..))
import TcEvidence (EvTerm(..))
import TcPluginM (TcPluginM)
import qualified TcPluginM (newGiven)
import Outputable (ppr)
import Type (PredType)

-- | Create a new [G]iven constraint, with the supplied evidence. This must not
-- be invoked from 'tcPluginInit' or 'tcPluginStop', or it will panic.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
newGiven loc pty (EvExpr ev) = TcPluginM.newGiven loc pty ev
newGiven _ _  ev = panicDoc "newGiven: not an EvExpr: " (ppr ev)

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
overEvidencePredType f (CQuantCan qci) =
  let
    ev :: CtEvidence
    ev = qci_ev qci
  in CQuantCan ( qci { qci_ev = ev { ctev_pred = f (ctev_pred ev) } } )
overEvidencePredType f ct =
  let
    ev :: CtEvidence
    ev = cc_ev ct
  in ct { cc_ev = ev { ctev_pred = f (ctev_pred ev) } }