{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Internal
  ( -- * Create new constraints
    TcPlugin.newWanted
  , newGiven
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

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Tc.Plugin (TcPluginM, lookupOrig, tcPluginTrace)
import qualified GHC.Tc.Plugin as TcPlugin
  (newWanted, getTopEnv, tcPluginIO, findImportedModule)
import GHC.Tc.Types (TcPlugin(..), TcPluginSolveResult(..))
import Control.Arrow (first, second)
import Data.Function (on)
import Data.List (groupBy, partition, sortOn)
import GHC.Tc.Utils.TcType (TcType)
import Data.Maybe (mapMaybe)

import GhcApi.Constraint (Ct(..))
import GhcApi.GhcPlugins

import Internal.Type (substType)
import Internal.Constraint (newGiven, flatToCt, mkSubst, overEvidencePredType)
import Internal.Evidence (evByFiat)

-- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module.
                           -- NOTE: This value is ignored on ghc>=8.0.
             -> TcPluginM Module
lookupModule mod_nm _pkg = do
  hsc_env <- TcPlugin.getTopEnv
  let fc         = hsc_FC hsc_env
      dflags     = hsc_dflags hsc_env
      fopts      = initFinderOpts dflags
      units      = hsc_units hsc_env
      mhome_unit = hsc_home_unit_maybe hsc_env
  found_module <- TcPlugin.tcPluginIO $ findPluginModule fc fopts units
                                          mhome_unit mod_nm
  case found_module of
    Found _ h -> return h
    _ -> do
      let pkg_qual = maybe NoPkgQual (ThisPkg . homeUnitId) mhome_unit
      found_module' <- TcPlugin.findImportedModule mod_nm pkg_qual
      case found_module' of
        Found _ h -> return h
        _ -> panicDoc "Couldn't find module" (ppr mod_nm)

-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName = lookupOrig

-- | Print out extra information about the initialisation, stop, and every run
-- of the plugin when @-ddump-tc-trace@ is enabled.
tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit    = traceInit
                                      , tcPluginSolve   = traceSolve
                                      , tcPluginRewrite = tcPluginRewrite
                                      , tcPluginStop    = traceStop
                                      }
  where
    traceInit = do
      tcPluginTrace ("tcPluginInit " ++ s) empty >> tcPluginInit

    traceStop  z = tcPluginTrace ("tcPluginStop " ++ s) empty >> tcPluginStop z

    traceSolve z ev given wanted = do
      tcPluginTrace ("tcPluginSolve start " ++ s)
                        (text "given   =" <+> ppr given
                      $$ text "wanted  =" <+> ppr wanted)
      r <- tcPluginSolve z ev given wanted
      case r of
        TcPluginOk solved new
          -> tcPluginTrace ("tcPluginSolve ok " ++ s)
                           (text "solved =" <+> ppr solved
                         $$ text "new    =" <+> ppr new)
        TcPluginContradiction bad
          -> tcPluginTrace ("tcPluginSolve contradiction " ++ s)
                           (text "bad =" <+> ppr bad)
        TcPluginSolveResult bad solved new
          -> tcPluginTrace ("tcPluginSolveResult " ++ s)
                           (text "solved =" <+> ppr solved
                         $$ text "bad    =" <+> ppr bad
                         $$ text "new    =" <+> ppr new)
      return r

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
