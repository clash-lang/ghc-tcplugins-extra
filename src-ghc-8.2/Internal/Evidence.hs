module Internal.Evidence (evByFiat, evByFiatWithDependencies) where

import TcEvidence (EvTerm(..))
import TyCoRep (UnivCoProvenance (..))

import GhcApi.GhcPlugins

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 =
  EvCoercion $ mkUnivCo (PluginProv name) Nominal t1 t2

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiatWithDependencies ::
  String -- ^ Name the coercion should have
  -> [Coercion] -- ^ The set of all the in-scope coercion that the proof makes use of.
  -> Type -- ^ The LHS of the equivalence relation (~)
  -> Type -- ^ The RHS of the equivalence relation (~)
  -> EvTerm
evByFiatWithDependencies name _deps t1 t2 =
  EvCoercion $ mkUnivCo (PluginProv name) Nominal t1 t2
