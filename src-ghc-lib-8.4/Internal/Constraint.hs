module Internal.Constraint (module TcPluginM, flatToCt) where

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
