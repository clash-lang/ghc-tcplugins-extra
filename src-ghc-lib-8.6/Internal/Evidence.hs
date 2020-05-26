module Internal.Evidence (evByFiat) where

import TcEvidence (EvTerm(..))
import TyCoRep (UnivCoProvenance (..))

import GhcApi.GhcPlugins

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 =
  EvExpr $ Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2
