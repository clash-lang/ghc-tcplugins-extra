module Internal.Evidence (evByFiat, evByFiatWithDependencies) where

import GHC.Tc.Types.Evidence (EvTerm(..))
import GHC.Core.TyCo.Rep (UnivCoProvenance (..))

import GhcApi.GhcPlugins

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 =
  EvExpr $ Coercion $ mkUnivCo (PluginProv name) [] Nominal t1 t2
{-# DEPRECATED evByFiat "'evByFiat' creates proofs that can lead to unsoundness, use 'evByFiatWithDependencies' instead.\nSee also https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12037" #-}

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiatWithDependencies ::
  String -- ^ Name the coercion should have
  -> [Coercion] -- ^ The set of all the in-scope coercion that the proof makes use of.
  -> Type -- ^ The LHS of the equivalence relation (~)
  -> Type -- ^ The RHS of the equivalence relation (~)
  -> EvTerm
evByFiatWithDependencies name deps t1 t2 =
  EvExpr $ Coercion $ mkUnivCo (PluginProv name) deps Nominal t1 t2
