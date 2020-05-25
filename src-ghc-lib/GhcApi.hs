{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GhcApi
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

import GhcPlugins hiding (TcPlugin, mkSubst)
import Coercion   (Role (..), mkUnivCo)
import FastString (FastString, fsLit)
import Module     (Module, ModuleName)
import Name       (Name)
import OccName    (OccName)
import Outputable (($$), (<+>), empty, ppr, text)
import Panic      (panicDoc)
import TcEvidence (EvTerm (..))
import TcPluginM  (FindResult (..), TcPluginM, lookupOrig, tcPluginTrace)
import qualified  TcPluginM
import qualified  Finder
import TcRnTypes  (TcPlugin (..), TcPluginResult (..))
import TyCoRep    (UnivCoProvenance (..))
import Type       (PredType, Type)
import Control.Arrow (first, second)
import Data.Function (on)
import Data.List     (groupBy, partition, sortOn)
import Constraint
  (Ct (..), CtEvidence (..), CtLoc, ctLoc, ctEvId, mkNonCanonical)
import TcType        (TcTyVar, TcType)
import Predicate     (mkPrimEqPred)
import Data.Maybe    (mapMaybe)
import TyCoRep       (Type (..))

pattern FoundModule :: Module -> FindResult
pattern FoundModule a <- Found _ a
fr_mod :: a -> a
fr_mod = id

-- | Create a new [W]anted constraint.
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
newWanted = TcPluginM.newWanted

-- | Create a new [G]iven constraint, with the supplied evidence. This must not
-- be invoked from 'tcPluginInit' or 'tcPluginStop', or it will panic.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
newGiven loc pty (EvExpr ev) = TcPluginM.newGiven loc pty ev
newGiven _ _  ev = panicDoc "newGiven: not an EvExpr: " (ppr ev)

-- | Create a new [D]erived constraint.
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
newDerived = TcPluginM.newDerived

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 =
  EvExpr $ Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2

-- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module.
                           -- NOTE: This value is ignored on ghc>=8.0.
             -> TcPluginM Module
lookupModule mod_nm _pkg = do
  hsc_env <- TcPluginM.getTopEnv
  found_module <- TcPluginM.tcPluginIO $ Finder.findPluginModule hsc_env mod_nm
  case found_module of
    FoundModule h -> return (fr_mod h)
    _          -> do
      found_module' <- TcPluginM.findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
        FoundModule h -> return (fr_mod h)

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
initializeStaticFlags = return ()

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
              (ctEvId ct)
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
mkSubst ct@(CTyEqCan {..})  = Just ((cc_tyvar,cc_rhs),ct)
mkSubst ct@(CFunEqCan {..}) = Just ((cc_fsk,TyConApp cc_fun cc_tyargs),ct)
mkSubst _                   = Nothing

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
substType subst (FunTy af t1 t2) =
  FunTy af (substType subst t1) (substType subst t2)
substType _ l@(LitTy _) = l
substType subst (CastTy ty co) =
  CastTy (substType subst ty) co
substType _ co@(CoercionTy _) = co
