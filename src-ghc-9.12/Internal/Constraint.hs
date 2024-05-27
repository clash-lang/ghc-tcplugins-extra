{-# LANGUAGE RecordWildCards #-}

module Internal.Constraint (newGiven, flatToCt, mkSubst, overEvidencePredType) where

import GhcApi.GhcPlugins
import GhcApi.Constraint
  (Ct(..), CtEvidence(..), CanEqLHS(..), CtLoc, ctLoc, ctEvId, mkNonCanonical)

import GHC.Tc.Utils.TcType (TcType)
import GHC.Tc.Types.Constraint (DictCt(..), IrredCt(..), EqCt(..), QCInst(..))
import GHC.Tc.Types.Evidence (EvTerm(..), EvBindsVar)
import GHC.Tc.Plugin (TcPluginM)
import qualified GHC.Tc.Plugin as TcPlugin (newGiven)

-- | Create a new [G]iven constraint, with the supplied evidence. This must not
-- be invoked from 'tcPluginInit' or 'tcPluginStop', or it will panic.
newGiven :: EvBindsVar -> CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
newGiven tcEvbinds loc pty (EvExpr ev) = TcPlugin.newGiven tcEvbinds loc pty ev
newGiven _ _ _ ev = panicDoc "newGiven: not an EvExpr: " (ppr ev)

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
mkSubst ct@(CEqCan (EqCt {..}))
  | TyVarLHS tyvar <- eq_lhs
  = Just ((tyvar,eq_rhs),ct)
mkSubst _ = Nothing

-- | Modify the predicate type of the evidence term of a constraint
overEvidencePredType :: (TcType -> TcType) -> Ct -> Ct
overEvidencePredType f (CDictCan di) =
  let
    ev :: CtEvidence
    ev = di_ev di
  in CDictCan ( di { di_ev = ev { ctev_pred = f (ctev_pred ev) } } )
overEvidencePredType f (CIrredCan ir) =
  let
    ev :: CtEvidence
    ev = ir_ev ir
  in CIrredCan ( ir { ir_ev = ev { ctev_pred = f (ctev_pred ev) } } )
overEvidencePredType f (CEqCan eq) =
  let
    ev :: CtEvidence
    ev = eq_ev eq
  in CEqCan ( eq { eq_ev = ev { ctev_pred = f (ctev_pred ev) } } )
overEvidencePredType f (CNonCanonical ct) =
  let
    ev :: CtEvidence
    ev = ct
  in CNonCanonical ( ev { ctev_pred = f (ctev_pred ev) } )
overEvidencePredType f (CQuantCan qci) =
  let
    ev :: CtEvidence
    ev = qci_ev qci
  in CQuantCan ( qci { qci_ev = ev { ctev_pred = f (ctev_pred ev) } } )
