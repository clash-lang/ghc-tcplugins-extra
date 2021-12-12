{-# LANGUAGE RecordWildCards #-}

module Internal.Constraint (newGiven, flatToCt, mkSubst, overEvidencePredType) where

import GhcApi.GhcPlugins
import GhcApi.Constraint
  (Ct(..), CtEvidence(..), CtLoc, ctLoc, ctEvId, mkNonCanonical)

import GHC.Tc.Utils.TcType (TcType)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Tc.Types.Constraint (QCInst(..))
import GHC.Tc.Types.Evidence (EvTerm(..))
import GHC.Tc.Plugin (TcPluginM)
import qualified GHC.Tc.Plugin as TcPlugin (newGiven)

-- | Create a new [G]iven constraint, with the supplied evidence. This must not
-- be invoked from 'tcPluginInit' or 'tcPluginStop', or it will panic.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
newGiven loc pty (EvExpr ev) = TcPlugin.newGiven loc pty ev
newGiven _ _  ev = panicDoc "newGiven: not an EvExpr: " (ppr ev)

flatToCt :: [((TcTyVar,TcType),Ct)] -> Maybe Ct
flatToCt [((_,lhs),ct),((_,rhs),_)]
    = Just
    $ mkNonCanonical
    $ CtGiven (mkPrimEqPred lhs rhs)
              (ctEvId ct)
              (ctLoc ct)

flatToCt _ = Nothing

-- | Create simple substitution from type equalities
mkSubst :: Ct -> Maybe ((TcTyVar, TcType),Ct)
mkSubst ct@(CTyEqCan {..})  = Just ((cc_tyvar,cc_rhs),ct)
mkSubst ct@(CFunEqCan {..}) = Just ((cc_fsk,TyConApp cc_fun cc_tyargs),ct)
mkSubst _ = Nothing

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
