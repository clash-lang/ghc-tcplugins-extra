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
    -- * Substitutions
  , flattenGivens
  , mkSubst
  , mkSubst'
  , substType
  , substCt
  )
where

-- External
import Data.Maybe (mapMaybe)
import Control.Arrow (first, second)
import Data.Function (on)
import Data.List     (groupBy, partition, sortOn)

-- GHC API
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Unit.Finder as Finder
import GHC.Tc.Types.Constraint (CanEqLHS (..))
#elif MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Finder as Finder
#endif
#if MIN_VERSION_ghc(9,0,0)
import GHC.Core (Expr (..))
import GHC.Core.Coercion (Role (..), mkPrimEqPred, mkUnivCo)
import GHC.Core.Type  (PredType)
import GHC.Core.TyCo.Rep (Type (..), UnivCoProvenance (..))
import GHC.Data.FastString (FastString, fsLit)
import GHC.Unit.Module (Module, ModuleName)
import GHC.Tc.Plugin (FindResult (..), TcPluginM, lookupOrig, tcPluginTrace)
import qualified GHC.Tc.Plugin as TcPluginM
import GHC.Tc.Utils.TcType (TcTyVar, TcType)
import GHC.Tc.Types (TcPlugin (..), TcPluginResult (..))
import GHC.Tc.Types.Constraint
  (Ct (..), CtLoc, CtEvidence (..), QCInst (..), ctEvId, ctLoc, mkNonCanonical)
import GHC.Tc.Types.Evidence (EvTerm (..))
import GHC.Types.Name (Name)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Utils.Outputable ((<+>), ($$), empty, ppr, text)
import GHC.Utils.Panic (panicDoc)
#else
#if __GLASGOW_HASKELL__ < 711
import BasicTypes (TopLevelFlag (..))
#endif
#if MIN_VERSION_ghc(8,5,0)
import CoreSyn    (Expr(..))
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
                   TcPlugin (..), TcPluginResult (..), ctEvLoc,
                   ctLocEnv, setCtLocEnv)
#else
import TcPluginM  (FindResult (..), TcPluginM, lookupOrig, tcPluginTrace)
import qualified  TcPluginM
import qualified  Finder
#if __GLASGOW_HASKELL__ < 809
import TcRnTypes  (CtEvidence (..), CtLoc,
                   TcPlugin (..), TcPluginResult (..))
#else
import TcRnTypes  (TcPlugin (..), TcPluginResult (..))
#endif
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
#if __GLASGOW_HASKELL__ < 809
import TcRnTypes     (Ct (..), ctLoc, ctEvId, mkNonCanonical
#if __GLASGOW_HASKELL__ >= 806
                     ,QCInst(..)
#endif
                    )
#else
import Constraint
  (Ct (..), CtEvidence (..), CtLoc, QCInst(..), ctLoc, ctEvId, mkNonCanonical)
#endif
import TcType        (TcTyVar, TcType)
#if __GLASGOW_HASKELL__ < 809
import Type          (mkPrimEqPred)
#else
import Predicate     (mkPrimEqPred)
#endif
#if __GLASGOW_HASKELL__ < 711
import TcRnTypes     (ctEvTerm)
import TypeRep       (Type (..))
#else
import TyCoRep       (Type (..))
#endif
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
#if MIN_VERSION_ghc(8,5,0)
newGiven loc pty (EvExpr ev) = TcPluginM.newGiven loc pty ev
newGiven _ _  ev = panicDoc "newGiven: not an EvExpr: " (ppr ev)
#elif __GLASGOW_HASKELL__ >= 711
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
evByFiat name t1 t2 =
#if MIN_VERSION_ghc(8,5,0)
                      EvExpr
                    $ Coercion
#else
                      EvCoercion
#if __GLASGOW_HASKELL__ < 711
                    $ TcCoercion
#endif
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
             -> FastString -- ^ Name of the package containing the module.
                           -- NOTE: This value is ignored on ghc>=8.0.
             -> TcPluginM Module
lookupModule mod_nm _pkg = do
#if __GLASGOW_HASKELL__ >= 711
  hsc_env <- TcPluginM.getTopEnv
  found_module <- TcPluginM.tcPluginIO $ Finder.findPluginModule hsc_env mod_nm
#else
  found_module <- findImportedModule mod_nm $ Just _pkg
#endif
  case found_module of
#if __GLASGOW_HASKELL__ >= 711
    FoundModule h -> return (fr_mod h)
#else
    Found _ md -> return md
#endif
    _          -> do
      found_module' <- TcPluginM.findImportedModule mod_nm $ Just $ fsLit "this"
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

-- | Flattens evidence of constraints by substituting each others equalities.
--
-- __NB:__ Should only be used on /[G]iven/ constraints!
--
-- __NB:__ Doesn't flatten under binders
flattenGivens
  :: [Ct]
  -> [Ct]
flattenGivens givens =
  mapMaybe flatToCt flat ++ map (substCt subst') givens
 where
  subst = mkSubst' givens
  (flat,subst')
    = second (map fst . concat)
    $ partition ((>= 2) . length)
    $ groupBy ((==) `on` (fst.fst))
    $ sortOn (fst.fst) subst

  flatToCt :: [((TcTyVar,TcType),Ct)] -> Maybe Ct
  flatToCt [((_,lhs),ct),((_,rhs),_)]
    = Just
    $ mkNonCanonical
    $ CtGiven (mkPrimEqPred lhs rhs)
#if MIN_VERSION_ghc(8,4,0)
              (ctEvId ct)
#elif MIN_VERSION_ghc(8,0,0)
              (ctEvId (cc_ev ct))
#else
              (ctEvTerm (cc_ev ct))
#endif
              (ctLoc ct)

  flatToCt _ = Nothing


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

-- | Create simple substitution from type equalities
mkSubst
  :: Ct
  -> Maybe ((TcTyVar, TcType),Ct)
#if MIN_VERSION_ghc(9,2,0)
mkSubst ct@(CEqCan {..})
  | TyVarLHS tyvar <- cc_lhs
  = Just ((tyvar,cc_rhs),ct)
#else
mkSubst ct@(CTyEqCan {..})  = Just ((cc_tyvar,cc_rhs),ct)
mkSubst ct@(CFunEqCan {..}) = Just ((cc_fsk,TyConApp cc_fun cc_tyargs),ct)
#endif
mkSubst _                   = Nothing

-- | Apply substitution in the evidence of Cts
substCt
  :: [(TcTyVar, TcType)]
  -> Ct
  -> Ct
substCt subst = overEvidencePredType (substType subst)

-- | Modify the predicate type of the evidence term of a constraint
overEvidencePredType :: (TcType -> TcType) -> Ct -> Ct
#if MIN_VERSION_ghc(8,6,0)
overEvidencePredType f (CQuantCan qci) =
  let
    ev :: CtEvidence
    ev = qci_ev qci
  in CQuantCan ( qci { qci_ev = ev { ctev_pred = f (ctev_pred ev) } } )
#endif
overEvidencePredType f ct =
  let
    ev :: CtEvidence
    ev = cc_ev ct
  in ct { cc_ev = ev { ctev_pred = f (ctev_pred ev) } }

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
#if __GLASGOW_HASKELL__ >= 900
substType subst (FunTy k1 k2 t1 t2) =
  FunTy k1 k2 (substType subst t1) (substType subst t2)
#elif __GLASGOW_HASKELL__ >= 809
substType subst (FunTy af t1 t2) =
  FunTy af (substType subst t1) (substType subst t2)
#elif __GLASGOW_HASKELL__ >= 802
substType subst (FunTy t1 t2) =
  FunTy (substType subst t1) (substType subst t2)
#elif __GLASGOW_HASKELL__ < 711
substType subst (FunTy t1 t2) =
  FunTy (substType subst t1) (substType subst t2)
#endif
substType _ l@(LitTy _) = l
#if __GLASGOW_HASKELL__ > 711
substType subst (CastTy ty co) =
  CastTy (substType subst ty) co
substType _ co@(CoercionTy _) = co
#endif
