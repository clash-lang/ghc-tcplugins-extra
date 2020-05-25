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

import GhcApi
