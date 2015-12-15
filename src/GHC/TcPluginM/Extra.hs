{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module GHC.TcPluginM.Extra
  ( -- * Create new constraints
    newWanted
  , newGiven
  , newDerived
  , newWantedWithProvenance
    -- * Creating evidence
  , evByFiat
    -- * Report contractions
  , failWithProvenace
    -- * Lookup
  , lookupModule
  , lookupName
    -- * Trace state of the plugin
  , tracePlugin
  )
where

-- External
import Data.Maybe (mapMaybe)

-- GHC API
import BasicTypes (TopLevelFlag (..))
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
                   tcPluginIO, tcPluginTrace, unsafeTcPluginTcM)
#else
import TcPluginM  (FindResult (..), TcPluginM, findImportedModule, lookupOrig,
                   tcPluginIO, tcPluginTrace, newEvVar, newCoercionHole)
import qualified  TcPluginM
#endif
import TcRnTypes  (Ct, CtEvidence (..), CtLoc, TcIdBinder (..), TcLclEnv (..),
                   TcPlugin (..), TcPluginResult (..), ctEvId, ctEvLoc, ctLoc,
                   ctLocEnv, mkNonCanonical, setCtLocEnv)
#if __GLASGOW_HASKELL__ >= 711
import TcRnTypes  (TcEvDest (..))
import TyCoRep    (UnivCoProvenance (..))
import Type       (isEqPred)
#endif
import Type       (EqRel (..), PredTree (..), PredType, Type, classifyPredType)
import Var        (varType)

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
import Data.IORef    (readIORef)
import Control.Monad (unless)
import StaticFlags   (initStaticOpts, v_opt_C_ready)


#if __GLASGOW_HASKELL__ >= 711
pattern FoundModule :: Module -> FindResult
pattern FoundModule a <- Found _ a
fr_mod :: a -> a
fr_mod = id
#endif


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
#if __GLASGOW_HASKELL__ >= 711
  d <- if isEqPred p then HoleDest <$> TcPluginM.newCoercionHole
                     else EvVarDest <$> TcPluginM.newEvVar p
  return CtWanted { ctev_pred = p
                  , ctev_dest = d
                  , ctev_loc  = loc'}
#else
  evVar <- unsafeTcPluginTcM $ newEvVar p
  return CtWanted { ctev_pred = p
                  , ctev_evar = evVar
                  , ctev_loc  = loc'}
#endif

newWantedWithProvenance ev _ =
  panicDoc "newWantedWithProvenance: not a Wanted: " (ppr ev)

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
#if __GLASGOW_HASKELL__ >= 711
    parents = map (\(id_,p) -> mkNonCanonical $ CtWanted p (EvVarDest id_) loc) eqBndrs
#else
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
initializeStaticFlags = tcPluginIO $ do
  r <- readIORef v_opt_C_ready
  unless r initStaticOpts
