{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Internal
  ( -- * Create new constraints
    newWanted
  , newGiven
  , newDerived
    -- * Creating evidence
  , evByFiat
    -- * Lookup
  , lookupModule
  , lookupName
    -- * Trace state of the plugin
  , tracePlugin
    -- * Substitutions
  , flattenGivens
  , mkSubst
  , mkSubst'
  , substType
  , substCt
  )
where

import GHC.Tc.Plugin (TcPluginM, lookupOrig, tcPluginTrace)
import qualified GHC.Tc.Plugin as TcPlugin
    (newDerived, newWanted, getTopEnv, tcPluginIO, findImportedModule)
import GHC.Tc.Types (TcPlugin(..), TcPluginResult(..))
import Control.Arrow (first, second)
import Data.Function (on)
import Data.List (groupBy, partition, sortOn)
import GHC.Tc.Utils.TcType (TcType)
import Data.Maybe (mapMaybe)

import GhcApi.Constraint (Ct(..), CtEvidence(..), CtLoc)
import GhcApi.GhcPlugins

import Internal.Type (substType)
import Internal.Constraint (newGiven, flatToCt, mkSubst, overEvidencePredType)
import Internal.Evidence (evByFiat)

{-# ANN fr_mod "HLint: ignore Use camelCase" #-}

pattern FoundModule :: Module -> FindResult
pattern FoundModule a <- Found _ a

fr_mod :: a -> a
fr_mod = id

-- | Create a new [W]anted constraint.
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
newWanted = TcPlugin.newWanted

-- | Create a new [D]erived constraint.
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
newDerived = TcPlugin.newDerived

-- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module.
                           -- NOTE: This value is ignored on ghc>=8.0.
             -> TcPluginM Module
lookupModule mod_nm _pkg = do
  hsc_env <- TcPlugin.getTopEnv
  found_module <- TcPlugin.tcPluginIO $ findPluginModule hsc_env mod_nm
  case found_module of
    FoundModule h -> return (fr_mod h)
    _          -> do
      found_module' <- TcPlugin.findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
        FoundModule h -> return (fr_mod h)
        _ -> panicDoc "Couldn't find module" (ppr mod_nm)

-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName = lookupOrig

-- | Print out extra information about the initialisation, stop, and every run
-- of the plugin when @-ddump-tc-trace@ is enabled.
tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit  = traceInit
                                      , tcPluginSolve = traceSolve
                                      , tcPluginStop  = traceStop
                                      }
  where
    traceInit = do
      -- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
      initializeStaticFlags
      tcPluginTrace ("tcPluginInit " ++ s) empty >> tcPluginInit

    traceStop  z = tcPluginTrace ("tcPluginStop " ++ s) empty >> tcPluginStop z

    traceSolve z given derived wanted = do
      tcPluginTrace ("tcPluginSolve start " ++ s)
                        (text "given   =" <+> ppr given
                      $$ text "derived =" <+> ppr derived
                      $$ text "wanted  =" <+> ppr wanted)
      r <- tcPluginSolve z given derived wanted
      case r of
        TcPluginOk solved new     -> tcPluginTrace ("tcPluginSolve ok " ++ s)
                                         (text "solved =" <+> ppr solved
                                       $$ text "new    =" <+> ppr new)
        TcPluginContradiction bad -> tcPluginTrace
                                         ("tcPluginSolve contradiction " ++ s)
                                         (text "bad =" <+> ppr bad)
      return r

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
initializeStaticFlags :: TcPluginM ()
initializeStaticFlags = return ()

-- | Flattens evidence of constraints by substituting each others equalities.
--
-- __NB:__ Should only be used on /[G]iven/ constraints!
--
-- __NB:__ Doesn't flatten under binders
flattenGivens :: [Ct] -> [Ct]
flattenGivens givens =
  mapMaybe flatToCt flat ++ map (substCt subst') givens
 where
  subst = mkSubst' givens
  (flat,subst')
    = second (map fst . concat)
    $ partition ((>= 2) . length)
    $ groupBy ((==) `on` (fst.fst))
    $ sortOn (fst.fst) subst

-- | Create flattened substitutions from type equalities, i.e. the substitutions
-- have been applied to each others right hand sides.
mkSubst' :: [Ct] -> [((TcTyVar,TcType),Ct)]
mkSubst' = foldr substSubst [] . mapMaybe mkSubst
 where
  substSubst :: ((TcTyVar,TcType),Ct)
             -> [((TcTyVar,TcType),Ct)]
             -> [((TcTyVar,TcType),Ct)]
  substSubst ((tv,t),ct) s = ((tv,substType (map fst s) t),ct)
                           : map (first (second (substType [(tv,t)]))) s

-- | Apply substitution in the evidence of Cts
substCt :: [(TcTyVar, TcType)] -> Ct -> Ct
substCt subst = overEvidencePredType (substType subst)
