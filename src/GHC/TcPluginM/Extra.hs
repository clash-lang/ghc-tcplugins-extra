{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module GHC.TcPluginM.Extra
  ( -- * Create new constraints
    newWantedWithProvenance
  , newSimpleGiven
  , evByFiat
    -- * Report contractions
  , failWithProvenace
    -- * Trace state of the plugin
  , tracePlugin
  )
where

-- External
import Data.Maybe (mapMaybe)

-- GHC API
import BasicTypes (TopLevelFlag (..))
import Coercion   (Role (..), mkUnivCo)
import FastString (fsLit)
import Outputable (($$), (<+>), empty, ppr, text)
import Panic      (panicDoc)
import TcEvidence (EvTerm (..), TcCoercion (..))
import TcMType    (newEvVar)
import TcPluginM  (TcPluginM, tcPluginIO, tcPluginTrace, unsafeTcPluginTcM)
import TcRnTypes  (Ct, CtEvidence (..), CtLoc, TcIdBinder (..), TcLclEnv (..),
                   TcPlugin (..), TcPluginResult (..), ctEvId, ctEvLoc, ctLoc,
                   ctLocEnv, mkNonCanonical, setCtLocEnv)
import Type       (EqRel (..), PredTree (..), PredType, Type, classifyPredType)
import Var        (varType)

-- workaround for https://ghc.haskell.org/trac/ghc/ticket/10301
import Data.IORef    (readIORef)
import Control.Monad (unless)
import StaticFlags   (initStaticOpts, v_opt_C_ready)

-- | Create a new [W]anted constraint that remembers from which wanted
-- constraint it was derived
newWantedWithProvenance :: CtEvidence    -- ^ Constraint from which the new
                                        -- wanted is derived
                        -> PredType     -- ^ The type of the new constraint
                        -> TcPluginM Ct -- ^ The new constraint
newWantedWithProvenance ev@(CtWanted {}) p = do
  let loc    = ctEvLoc ev
      env    = ctLocEnv loc
      id_    = ctEvId ev
      env'   = env {tcl_bndrs = (TcIdBndr id_ NotTopLevel):tcl_bndrs env}
      loc'   = setCtLocEnv loc env'
  evVar <- unsafeTcPluginTcM $ newEvVar p
  let ev' = CtWanted {ctev_pred = p, ctev_evar = evVar, ctev_loc = loc'}
      ct = mkNonCanonical ev'
  return ct
newWantedWithProvenance ev _ =
  panicDoc "newWantedWithProvenance: not a Wanted: " (ppr ev)

newSimpleGiven :: String -> CtLoc -> PredType -> (Type,Type) -> TcPluginM Ct
newSimpleGiven name loc predicate (ty1,ty2) = do
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 711
  ev <- unsafeTcPluginTcM $ newEvVar predicate
#endif
  let ctE = CtGiven { ctev_pred = predicate
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 711
                    , ctev_evar = ev
#else
                    , ctev_evtm = evByFiat name (ty1, ty2)
#endif
                    , ctev_loc  = loc
                    }
      ct  = mkNonCanonical ctE
  return ct

-- | The 'EvTerm' equivalent for 'Unsafe.unsafeCoerce'
evByFiat :: String       -- ^ Name the coercion should have
         -> (Type, Type) -- ^ The LHS and RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name (t1,t2) = EvCoercion $ TcCoercion
                      $ mkUnivCo (fsLit name) Nominal t1 t2

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
