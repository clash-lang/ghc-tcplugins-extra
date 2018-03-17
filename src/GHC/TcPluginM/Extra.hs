{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TcPluginM.Extra
  ( -- * Create new constraints
    newWanted
  , newGiven
  , newDerived
#if __GLASGOW_HASKELL__ < 711
  , newWantedWithProvenance
#endif
    -- * Creating evidence
  , evByFiat
#if __GLASGOW_HASKELL__ < 711
    -- * Report contractions
  , failWithProvenace
#endif
    -- * Lookup
  , lookupModule
  , lookupName
    -- * Trace state of the plugin
  , tracePlugin
#if __GLASGOW_HASKELL__ > 804
    -- * Substitutions
  , flattenGivens
  , mkSubst
  , mkSubst'
  , substType
  , substCt
#endif
  )
where

-- External
#if __GLASGOW_HASKELL__ < 711
import Data.Maybe (mapMaybe)
#endif

-- GHC API
#if __GLASGOW_HASKELL__ < 711
import BasicTypes (TopLevelFlag (..))
#endif
import Coercion   (Role (..), mkUnivCo)
import FastString (FastString, fsLit)
import Module     (Module, ModuleName)
import Name       (Name)
import OccName    (OccName)
import Outputable (($$), (<+>), empty, ppr, text)
import Panic      (panicDoc)
#if __GLASGOW_HASKELL__ >= 711
import TcEvidence (EvTerm (..))
#else
import TcEvidence (EvTerm (..), TcCoercion (..))
import TcMType    (newEvVar)
#endif
#if __GLASGOW_HASKELL__ < 711
import TcPluginM  (FindResult (..), TcPluginM, findImportedModule, lookupOrig,
                   tcPluginTrace, unsafeTcPluginTcM)
import TcRnTypes  (Ct, CtEvidence (..), CtLoc, TcIdBinder (..), TcLclEnv (..),
                   TcPlugin (..), TcPluginResult (..), ctEvId, ctEvLoc, ctLoc,
                   ctLocEnv, mkNonCanonical, setCtLocEnv)
#else
import TcPluginM  (FindResult (..), TcPluginM, findImportedModule, lookupOrig,
                   tcPluginTrace)
import qualified  TcPluginM
import TcRnTypes  (CtEvidence (..), CtLoc,
                   TcPlugin (..), TcPluginResult (..))
#endif
#if __GLASGOW_HASKELL__ < 802
import TcPluginM  (tcPluginIO)
#endif
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (UnivCoProvenance (..))
import Type       (PredType, Type)
#else
import Type       (EqRel (..), PredTree (..), PredType, Type, classifyPredType)
import Var        (varType)
#endif
#if __GLASGOW_HASKELL__ >= 804
import Control.Arrow (second)
import Data.Maybe    (mapMaybe)
import TcRnTypes     (Ct (..))
import TcType        (TcTyVar, TcType)
import TyCoRep       (Type (..))
#endif

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
#if __GLASGOW_HASKELL__ < 802
import Data.IORef    (readIORef)
import Control.Monad (unless)
import StaticFlags   (initStaticOpts, v_opt_C_ready)
#endif

#if __GLASGOW_HASKELL__ >= 711
pattern FoundModule :: Module -> FindResult
pattern FoundModule a <- Found _ a
fr_mod :: a -> a
fr_mod = id
#endif


#if __GLASGOW_HASKELL__ < 711
{-# DEPRECATED newWantedWithProvenance "No longer available in GHC 8.0+" #-}
-- | Create a new [W]anted constraint that remembers from which wanted
-- constraint it was derived
newWantedWithProvenance :: CtEvidence -- ^ Constraint from which the new
                                      -- wanted is derived
                        -> PredType   -- ^ The type of the new constraint
                        -> TcPluginM CtEvidence
newWantedWithProvenance ev@(CtWanted {}) p = do
  let loc    = ctEvLoc ev
      env    = ctLocEnv loc
      id_    = ctEvId ev
      env'   = env {tcl_bndrs = (TcIdBndr id_ NotTopLevel):tcl_bndrs env}
      loc'   = setCtLocEnv loc env'
  evVar <- unsafeTcPluginTcM $ newEvVar p
  return CtWanted { ctev_pred = p
                  , ctev_evar = evVar
                  , ctev_loc  = loc'}

newWantedWithProvenance ev _ =
  panicDoc "newWantedWithProvenance: not a Wanted: " (ppr ev)
#endif

-- | Create a new [W]anted constraint.
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
#if __GLASGOW_HASKELL__ >= 711
newWanted = TcPluginM.newWanted
#else
newWanted loc pty = do
    new_ev <- unsafeTcPluginTcM $ newEvVar pty
    return CtWanted { ctev_pred = pty
                    , ctev_evar = new_ev
                    , ctev_loc  = loc
                    }
#endif

-- | Create a new [G]iven constraint, with the supplied evidence. This must not
-- be invoked from 'tcPluginInit' or 'tcPluginStop', or it will panic.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
#if __GLASGOW_HASKELL__ >= 711
newGiven = TcPluginM.newGiven
#else
newGiven loc pty evtm = return
  CtGiven { ctev_pred = pty
          , ctev_evtm = evtm
          , ctev_loc  = loc
          }
#endif

-- | Create a new [D]erived constraint.
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
#if __GLASGOW_HASKELL__ >= 711
newDerived = TcPluginM.newDerived
#else
newDerived loc pty = return
  CtDerived { ctev_pred = pty
            , ctev_loc  = loc
            }
#endif

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 = EvCoercion
#if __GLASGOW_HASKELL__ < 711
                    $ TcCoercion
#endif
                    $ mkUnivCo
#if __GLASGOW_HASKELL__ >= 711
                        (PluginProv name)
#else
                        (fsLit name)
#endif
                        Nominal t1 t2

#if __GLASGOW_HASKELL__ < 711
{-# DEPRECATED failWithProvenace "No longer available in GHC 8.0+" #-}
-- | Mark the given constraint as insoluble.
--
-- If the [W]anted constraint was made by 'newWantedWithProvenance', it will
-- also mark the parent(s) from which the constraint was derived as insoluble.
-- Even if they were previously assumed to be solved.
failWithProvenace :: Ct -> TcPluginM TcPluginResult
failWithProvenace ct = return (TcPluginContradiction (ct : parents))
  where
    loc      = ctLoc ct
    lclbndrs = mapMaybe (\case {TcIdBndr id_ NotTopLevel -> Just id_
                               ;_ -> Nothing })
             $ tcl_bndrs (ctLocEnv loc)
    eqBndrs = filter ((\x -> case x of { EqPred NomEq _ _ -> True
                                       ; _ -> False })
                      . classifyPredType . snd)
            $ map (\ev -> (ev,varType ev)) lclbndrs
    parents = map (\(id_,p) -> mkNonCanonical $ CtWanted p id_ loc) eqBndrs
#endif

-- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module
             -> TcPluginM Module
lookupModule mod_nm pkg = do
  found_module <- findImportedModule mod_nm $ Just pkg
  case found_module of
#if __GLASGOW_HASKELL__ >= 711
    FoundModule h -> return (fr_mod h)
#else
    Found _ md -> return md
#endif
    _          -> do
      found_module' <- findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
#if __GLASGOW_HASKELL__ >= 711
        FoundModule h -> return (fr_mod h)
#else
        Found _ md -> return md
#endif
        _          -> panicDoc "Unable to resolve module looked up by plugin: "
                               (ppr mod_nm)

-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ

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
#if __GLASGOW_HASKELL__ < 802
initializeStaticFlags = tcPluginIO $ do
  r <- readIORef v_opt_C_ready
  unless r initStaticOpts
#else
initializeStaticFlags = return ()
#endif

#if __GLASGOW_HASKELL__ > 804
-- | Flattens evidence of constraints by substituting each others equalities.
--
-- __NB:__ Should only be used on /[G]iven/ constraints!
-- __NB:__ Doesn't flatten under binders
flattenGivens
  :: [Ct]
  -> [Ct]
flattenGivens givens = map (substCt subst) givens
 where
  subst = mkSubst' givens

-- | Create flattened substitutions from type equalities, i.e. the substitutions
-- have been applied to each others right hand sides.
mkSubst' :: [Ct] -> [(TcTyVar,TcType)]
mkSubst' = foldr substSubst [] . mapMaybe mkSubst
 where
  substSubst (tv,t) s = (tv,substType s t) : map (second (substType [(tv,t)])) s

-- | Create simple substitution from type equalities
mkSubst
  :: Ct
  -> Maybe (TcTyVar, TcType)
mkSubst (CTyEqCan {..})  = Just (cc_tyvar,cc_rhs)
mkSubst (CFunEqCan {..}) = Just (cc_fsk,TyConApp cc_fun cc_tyargs)
mkSubst _                = Nothing

-- | Apply substitution in the evidence of Cts
substCt
  :: [(TcTyVar, TcType)]
  -> Ct
  -> Ct
substCt subst ct =
  ct { cc_ev = (cc_ev ct) {ctev_pred = substType subst (ctev_pred (cc_ev ct))}
     }

-- | Apply substitutions in Types
--
-- __NB:__ Doesn't substitute under binders
substType
  :: [(TcTyVar, TcType)]
  -> TcType
  -> TcType
substType subst tv@(TyVarTy v) = case lookup v subst of
  Just t  -> t
  Nothing -> tv
substType subst (AppTy t1 t2) =
  AppTy (substType subst t1) (substType subst t2)
substType subst (TyConApp tc xs) =
  TyConApp tc (map (substType subst) xs)
substType _subst t@(ForAllTy _tv _ty) =
  -- TODO: Is it safe to do "dumb" substitution under binders?
  -- ForAllTy tv (substType subst ty)
  t
substType subst (FunTy t1 t2) =
  FunTy (substType subst t1) (substType subst t2)
substType _ l@(LitTy _) = l
substType subst (CastTy ty co) =
  CastTy (substType subst ty) co
substType _ co@(CoercionTy _) = co
#endif
